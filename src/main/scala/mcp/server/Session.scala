package mcp.server

import mcp.*

import scala.collection.mutable
import scala.util.control.NonFatal

/** A server-side MCP framework over the `mcp` datatypes: a transport-independent session core,
  * the stdio transport, and the Streamable HTTP transport.
  *
  * Deliberately out of scope, and documented as such rather than merely missing: any wiring into
  * athame's CLI or its sync tools, typed wrappers for server-initiated sampling and elicitation
  * (the correlation plumbing they will sit on is here, exercised by `ping`), SSE resumability via
  * `Last-Event-ID`, strict `Accept` negotiation, answering a POST with an SSE stream, TLS and
  * authorization, tool-list pagination and `notifications/tools/list_changed`, and session expiry.
  */
enum SessionState:
  /** Nothing has happened yet. Only `initialize` and `ping` are answered. */
  case New

  /** `initialize` answered, waiting for the client's `notifications/initialized`. */
  case Initializing

  /** The handshake is complete and every declared capability is open for business. */
  case Ready

/** Why a server-initiated request did not produce a result.
  *
  * Split because the two mean different things to the caller: [[Returned]] is the peer declining,
  * which is a normal protocol outcome, while [[Undecodable]] is the peer answering with something
  * that is not the result we asked for, which is the peer being broken.
  */
enum CallFailure:
  case Returned(error: JsonRpc.Error)
  case Undecodable(error: DecodeError)

  def message: String =
    this match
      case Returned(error)   => error.message
      case Undecodable(error) => error.render

/** One MCP conversation, from `initialize` to the last message, with no idea what it is speaking
  * over.
  *
  * ==Why `receive` returns replies instead of sinking them==
  *
  * [[receive]] is synchronous and total: text in, the replies that text directly earned out. The
  * sink passed to the constructor carries *only* server-initiated traffic — correlation requests,
  * log and progress notifications. That split is the whole reason the HTTP transport can work: a
  * reply belongs on the POST response that carried its request, while sink traffic has to go
  * somewhere else entirely (the GET stream). A design that pushed everything through one sink would
  * make that impossible to reassemble. stdio, which has only one pipe, simply writes both.
  *
  * Nothing in here does I/O, reads a clock, or generates randomness, so a session is a pure
  * function of the messages it has seen — which is what makes the suite able to pin whole
  * conversations byte for byte.
  */
final class ServerSession(
  val serverInfo: Implementation,
  declared: ServerCapabilities,
  toolProvider: Option[ToolProvider],
  send: String => Unit,
  instructions: Option[String] = None
):

  /** The capabilities this session actually advertises.
    *
    * `tools` is derived rather than trusted: it is declared exactly when a [[ToolProvider]] was
    * supplied, so the handshake cannot promise tools that no one can serve, nor hide tools that
    * can be called. `listChanged` is not offered — the registry is fixed for a session's life.
    */
  val capabilities: ServerCapabilities =
    declared.copy(tools = toolProvider.map(_ => ToolsCapability(None)))

  private var currentState: SessionState = SessionState.New
  private var minimumLevel: Option[LoggingLevel] = None
  private var clientImplementation: Option[Implementation] = None
  private var nextRequestId: Double = 1
  private val pending = mutable.Map.empty[RequestId, Either[JsonRpc.Error, Json.Obj] => Unit]

  def state: SessionState = currentState

  /** The level set by `logging/setLevel`, or `None` while the client has not chosen one. */
  def loggingLevel: Option[LoggingLevel] = minimumLevel

  /** Who the client said it was, once `initialize` has been answered. */
  def clientInfo: Option[Implementation] = clientImplementation

  // -----------------------------------------------------------------------------------------
  // Receiving
  // -----------------------------------------------------------------------------------------

  /** Handle one message and answer it.
    *
    * The result is the direct replies — at most one, since MCP has no batching — already encoded.
    * A notification, or a response to something we asked, earns no reply and returns empty.
    */
  def receive(text: String): Vector[String] =
    Codec.decodeClientMessage(text) match
      case Right(message) => accept(message)
      case Left(failure)  => refuse(text, failure)

  private def accept(message: ClientMessage): Vector[String] =
    message match
      case ClientMessage.Request(id, request) => Vector(answer(id, request))
      case ClientMessage.Notification(notification) =>
        note(notification)
        Vector.empty
      case ClientMessage.Response(id, result) =>
        settle(id, Right(result))
        Vector.empty
      case ClientMessage.Error(id, error) =>
        // An error response with no id answers nothing we can find, so there is nobody to tell.
        id.foreach(known => settle(known, Left(error)))
        Vector.empty

  private def answer(id: RequestId, request: ClientRequest): String =
    try
      request match
        // Answered in every state, by the spec's explicit instruction: a peer checking whether we
        // are alive should not have to have finished a handshake first.
        case ClientRequest.Ping(_)            => reply(id, ServerResult.Empty(EmptyResult.empty))
        case ClientRequest.Initialize(params) => initialize(id, params)
        case ClientRequest.SetLevel(params)   => whenReady(id)(setLevel(id, params))
        case ClientRequest.ListTools(_)       => whenReady(id)(listTools(id))
        case ClientRequest.CallTool(params)   => whenReady(id)(callTool(id, params))
    catch
      case NonFatal(thrown) =>
        // A handler that threw told us nothing useful about the client's request, so the client
        // learns only that we broke. The stack trace stays on this side of the wire.
        fail(id, ErrorCode.InternalError, describe(thrown))

  private def whenReady(id: RequestId)(handle: => String): String =
    if currentState == SessionState.Ready then handle
    else fail(id, ErrorCode.InvalidRequest, "Server is not initialized")

  private def note(notification: ClientNotification): Unit =
    notification match
      // The one notification that moves the machine. Arriving in any other state is a client that
      // has lost the thread, and is dropped rather than argued with — notifications get no reply.
      case ClientNotification.Initialized(_) =>
        if currentState == SessionState.Initializing then currentState = SessionState.Ready

      // Accepted and ignored. Dispatch is synchronous, so by the time a cancellation is read the
      // request it names has already been answered; there is never anything in flight to cancel.
      case ClientNotification.Cancelled(_) => ()

      // Accepted and ignored. Progress flows to whoever holds the token, and no server-initiated
      // request in this increment sends one, so no client update can match a pending call.
      case ClientNotification.Progress(_) => ()

  // -----------------------------------------------------------------------------------------
  // Handlers
  // -----------------------------------------------------------------------------------------

  private def initialize(id: RequestId, params: InitializeRequestParams): String =
    if currentState != SessionState.New then
      fail(id, ErrorCode.InvalidRequest, "Already initialized")
    else
      currentState = SessionState.Initializing
      clientImplementation = Some(params.clientInfo)
      // Both the echo case and the downgrade case land on the same string, because this server
      // speaks one revision. The client reads the answer and decides whether it can live with it.
      val result =
        InitializeResult(None, Mcp.LatestProtocolVersion, capabilities, serverInfo, instructions)
      reply(id, ServerResult.Initialize(result))

  /** `logging/setLevel`.
    *
    * A server that never declared the logging capability is answered with -32600 rather than
    * -32601: the method exists and we understand it perfectly well, we just never offered it. That
    * is a bad request, not a missing method.
    */
  private def setLevel(id: RequestId, params: SetLevelRequestParams): String =
    if capabilities.logging.isEmpty then
      fail(id, ErrorCode.InvalidRequest, "Logging capability not declared")
    else
      minimumLevel = Some(params.level)
      reply(id, ServerResult.Empty(EmptyResult.empty))

  private def listTools(id: RequestId): String =
    // No pagination in v1: any cursor the client sends is ignored and no nextCursor comes back,
    // which is the one-page case of the same protocol.
    withTools(id)(provider => reply(id, ServerResult.ListTools(ListToolsResult(None, None, provider.list()))))

  private def callTool(id: RequestId, params: CallToolRequestParams): String =
    withTools(id): provider =>
      if !provider.list().exists(_.name == params.name) then
        // A tool that does not exist is a protocol error, not a failed call: there was nothing to
        // run, so there is no result for the model to learn from.
        fail(id, ErrorCode.InvalidParams, s"Unknown tool: ${params.name}")
      else
        val context = new Context(ProgressToken.from(params.meta))
        provider.call(params.name, params.arguments, context) match
          case Right(result) => reply(id, ServerResult.CallTool(result))
          case Left(reason)  => fail(id, ErrorCode.InternalError, reason)

  /** Same reasoning as [[setLevel]]: the method is known, the capability was never offered. */
  private def withTools(id: RequestId)(handle: ToolProvider => String): String =
    toolProvider match
      case Some(provider) => handle(provider)
      case None           => fail(id, ErrorCode.InvalidRequest, "Tools capability not declared")

  // -----------------------------------------------------------------------------------------
  // Server-initiated requests
  // -----------------------------------------------------------------------------------------

  /** Ask the client something and hear back later.
    *
    * Ids come from a counter this session owns. JSON-RPC gives each direction its own id space, so
    * these never collide with the client's, however it numbers its own requests.
    *
    * The callback runs synchronously inside the [[receive]] that carried the answer.
    */
  def request[A](outgoing: ServerRequest, decoder: Decoder[A])(
    onResult: Either[CallFailure, A] => Unit
  ): Unit =
    val id = RequestId.Num(nextRequestId)
    nextRequestId += 1
    pending(id) =
      case Left(error) => onResult(Left(CallFailure.Returned(error)))
      case Right(result) =>
        decoder.decode(Path.root, Json.toJs(result)) match
          case Right(value)  => onResult(Right(value))
          case Left(failure) => onResult(Left(CallFailure.Undecodable(failure)))
    send(Codec.encodeServerMessage(ServerMessage.Request(id, outgoing)))

  /** Check that the client is still there. */
  def ping(onResult: Either[CallFailure, Unit] => Unit): Unit =
    request(ServerRequest.Ping(None), EmptyResult.decoder)(outcome => onResult(outcome.map(_ => ())))

  /** How many server-initiated requests are still waiting for an answer. */
  def pendingRequests: Int = pending.size

  /** A response we cannot match names a request we never made, or one already settled — a
    * duplicate, or a confused peer. Dropped: there is no one to hand it to and nothing to say back.
    */
  private def settle(id: RequestId, outcome: Either[JsonRpc.Error, Json.Obj]): Unit =
    pending.remove(id).foreach(callback => callback(outcome))

  // -----------------------------------------------------------------------------------------
  // Logging
  // -----------------------------------------------------------------------------------------

  /** Emit `notifications/message`, if the client's chosen level admits it.
    *
    * Until the client sets a level there is no filter at all — a server with nothing configured
    * says everything, rather than silently saying nothing.
    */
  def log(level: LoggingLevel, logger: Option[String], data: Json): Unit =
    if admits(level) then
      val params = LoggingMessageNotificationParams(None, level, logger, data)
      send(Codec.encodeServerMessage(ServerMessage.Notification(ServerNotification.LoggingMessage(params))))

  private def admits(level: LoggingLevel): Boolean =
    minimumLevel.forall(minimum => level.severity >= minimum.severity)

  private final class Context(val progressToken: Option[ProgressToken]) extends ToolContext:
    def progress(current: Double, total: Option[Double], message: Option[String]): Unit =
      progressToken.foreach: token =>
        val params = ProgressNotificationParams(None, token, current, total, message)
        send(Codec.encodeServerMessage(ServerMessage.Notification(ServerNotification.Progress(params))))

    def log(level: LoggingLevel, logger: Option[String], data: Json): Unit =
      ServerSession.this.log(level, logger, data)

  // -----------------------------------------------------------------------------------------
  // Refusals
  // -----------------------------------------------------------------------------------------

  /** Who a refusal is addressed to, recovered from text that failed to decode as a message.
    *
    * Typed decoding gets us nothing here — it already failed — so this is a deliberate second look
    * at the raw envelope, asking only whether there is an id to answer to. It runs on the error
    * path alone, so the second parse costs nothing anyone will notice.
    */
  private enum Attribution:
    /** A request with a usable id: it gets a proper error response. */
    case ToRequest(id: RequestId)

    /** A string method with no id — a notification. JSON-RPC forbids replying to one at all, even
      * when its params are the thing that failed to decode.
      */
    case ToNotification

    /** No id we can echo and no method either: an id of the wrong shape, or something that is not
      * a JSON-RPC message. Answered with a null id, which is what JSON-RPC reserves the null id
      * for.
      */
    case Unattributable

  private val attribution: Decoder[Attribution] = Decode.obj: fields =>
    // `present` treats an explicit null as absent, so `"id": null` reads as a notification. JSON-RPC
    // reserves the null id for responses, so a request that sends one is not owed an answer either.
    if fields.present("id") then
      Right(fields.req("id", RequestId.decoder) match
        case Right(id) => Attribution.ToRequest(id)
        case Left(_)   => Attribution.Unattributable)
    // A *string* method, specifically. `{"jsonrpc":"2.0","method":1}` is JSON-RPC's own example of
    // an invalid request, not a notification, and answering it with a null id is what the spec
    // shows. Testing only for the member's presence would silently drop it instead.
    else if fields.req("method", Decode.string).isRight then Right(Attribution.ToNotification)
    else Right(Attribution.Unattributable)

  private def addressee(text: String): Attribution =
    Codec.decode(text, attribution).getOrElse(Attribution.Unattributable)

  private def refuse(text: String, failure: DecodeError): Vector[String] =
    failure match
      // Nothing parsed, so there is nothing to read an id out of.
      case DecodeError.Malformed(_) =>
        Vector(anonymous(ErrorCode.ParseError, "Parse error", failure))

      case DecodeError.UnknownMethod(_, method) =>
        answerable(text, failure, ErrorCode.MethodNotFound, s"Method not found: $method")

      // A shape we could not read. The envelope decoded far enough to find an id, so the client
      // can be told which of its requests we could not understand.
      case _ =>
        answerable(text, failure, ErrorCode.InvalidParams, "Invalid params")

  private def answerable(
    text: String,
    failure: DecodeError,
    code: Double,
    message: String
  ): Vector[String] =
    addressee(text) match
      case Attribution.ToRequest(id)  => Vector(fail(id, code, message, Some(detail(failure))))
      case Attribution.ToNotification => Vector.empty
      case Attribution.Unattributable =>
        Vector(anonymous(ErrorCode.InvalidRequest, "Invalid Request", failure))

  private def detail(failure: DecodeError): Json = Json.Str(failure.render)

  private def reply(id: RequestId, result: ServerResult): String =
    Codec.encodeServerMessage(ServerMessage.response(id, result))

  private def fail(id: RequestId, code: Double, message: String, data: Option[Json] = None): String =
    Codec.encodeServerMessage(ServerMessage.Error(Some(id), JsonRpc.Error(code, message, data)))

  /** A refusal addressed to nobody: JSON-RPC's null id, which the datatypes now write (ruling M12).
    *
    * There is no special path here any more. `ServerMessage.Error(None, _)` encodes the explicit
    * `"id": null` that JSON-RPC 2.0 requires when the request could not be identified, so this
    * builds a message like every other reply does.
    */
  private def anonymous(code: Double, message: String, failure: DecodeError): String =
    Codec.encodeServerMessage(
      ServerMessage.Error(None, JsonRpc.Error(code, message, Some(detail(failure))))
    )

  /** What a thrown handler is willing to say for itself, with no stack trace. */
  private def describe(thrown: Throwable): String =
    Option(thrown.getMessage).filter(_.nonEmpty).getOrElse(thrown.getClass.getName)
