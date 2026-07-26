package mcp

import scala.scalajs.js

/** The direction unions and the envelope around them (schema.ts:124-170, 2494-2573).
  *
  * The schema's `ClientRequest`, `ServerNotification` and the rest are listed here restricted to
  * the members this package models; the omissions are named on each union. A method outside that
  * restriction — including one MCP defines but we skip — decodes to [[DecodeError.UnknownMethod]],
  * which is precisely the situation a dispatcher answers with `-32601`.
  *
  * A response's `result` stops at [[Json.Obj]]. Which result type a payload should be read as
  * depends on which request it answers, and that correlation belongs to a session layer this
  * increment does not build; the typed decoders it will need are on the result types themselves
  * ([[InitializeResult.decoder]], [[CallToolResult.decoder]], and so on).
  */

/** The `method` strings this package answers to. */
object Method:
  val Ping: String = "ping"
  val Initialize: String = "initialize"
  val SetLevel: String = "logging/setLevel"
  val CallTool: String = "tools/call"
  val ListTools: String = "tools/list"
  val CreateMessage: String = "sampling/createMessage"
  val Elicit: String = "elicitation/create"
  val Cancelled: String = "notifications/cancelled"
  val Progress: String = "notifications/progress"
  val Initialized: String = "notifications/initialized"
  val LoggingMessage: String = "notifications/message"
  val ToolListChanged: String = "notifications/tools/list_changed"
  val ElicitationComplete: String = "notifications/elicitation/complete"

/** A request a client sends (schema.ts:2494-2513).
  *
  * Omitted, being out of scope: `completion/complete`, `prompts/get`, `prompts/list`,
  * `resources/list`, `resources/templates/list`, `resources/read`, `resources/subscribe`,
  * `resources/unsubscribe`, and the four tasks requests (`tasks/get`, `tasks/result`,
  * `tasks/cancel`, `tasks/list`).
  */
enum ClientRequest:
  case Ping(params: Option[RequestParams])
  case Initialize(params: InitializeRequestParams)
  case SetLevel(params: SetLevelRequestParams)
  case CallTool(params: CallToolRequestParams)
  case ListTools(params: Option[PaginatedRequestParams])

  def method: String =
    this match
      case Ping(_)       => Method.Ping
      case Initialize(_) => Method.Initialize
      case SetLevel(_)   => Method.SetLevel
      case CallTool(_)   => Method.CallTool
      case ListTools(_)  => Method.ListTools

object ClientRequest:
  private[mcp] def read(fields: Fields, method: String): Either[DecodeError, ClientRequest] =
    method match
      case Method.Ping =>
        fields.opt("params", RequestParams.decoder).map(ClientRequest.Ping.apply)
      case Method.Initialize =>
        fields.req("params", InitializeRequestParams.decoder).map(ClientRequest.Initialize.apply)
      case Method.SetLevel =>
        fields.req("params", SetLevelRequestParams.decoder).map(ClientRequest.SetLevel.apply)
      case Method.CallTool =>
        fields.req("params", CallToolRequestParams.decoder).map(ClientRequest.CallTool.apply)
      case Method.ListTools =>
        fields.opt("params", PaginatedRequestParams.decoder).map(ClientRequest.ListTools.apply)
      case other => Left(DecodeError.UnknownMethod(fields.path / "method", other))

  private[mcp] def write(request: ClientRequest, out: JsObj): Unit =
    request match
      case ClientRequest.Ping(params) =>
        out.putOpt("params", params, RequestParams.encoder)
      case ClientRequest.Initialize(params) =>
        out.put("params", params, InitializeRequestParams.encoder)
      case ClientRequest.SetLevel(params) =>
        out.put("params", params, SetLevelRequestParams.encoder)
      case ClientRequest.CallTool(params) =>
        out.put("params", params, CallToolRequestParams.encoder)
      case ClientRequest.ListTools(params) =>
        out.putOpt("params", params, PaginatedRequestParams.encoder)

/** A notification a client sends (schema.ts:2515-2521).
  *
  * Omitted, being out of scope: `notifications/roots/list_changed` and `notifications/tasks/status`.
  */
enum ClientNotification:
  case Cancelled(params: CancelledNotificationParams)
  case Progress(params: ProgressNotificationParams)
  case Initialized(params: Option[NotificationParams])

  def method: String =
    this match
      case Cancelled(_)   => Method.Cancelled
      case Progress(_)    => Method.Progress
      case Initialized(_) => Method.Initialized

object ClientNotification:
  private[mcp] def read(fields: Fields, method: String): Either[DecodeError, ClientNotification] =
    method match
      case Method.Cancelled =>
        fields.req("params", CancelledNotificationParams.decoder).map(ClientNotification.Cancelled.apply)
      case Method.Progress =>
        fields.req("params", ProgressNotificationParams.decoder).map(ClientNotification.Progress.apply)
      case Method.Initialized =>
        fields.opt("params", NotificationParams.decoder).map(ClientNotification.Initialized.apply)
      case other => Left(DecodeError.UnknownMethod(fields.path / "method", other))

  private[mcp] def write(notification: ClientNotification, out: JsObj): Unit =
    notification match
      case ClientNotification.Cancelled(params) =>
        out.put("params", params, CancelledNotificationParams.encoder)
      case ClientNotification.Progress(params) =>
        out.put("params", params, ProgressNotificationParams.encoder)
      case ClientNotification.Initialized(params) =>
        out.putOpt("params", params, NotificationParams.encoder)

/** A result a client returns (schema.ts:2523-2532).
  *
  * Omitted, being out of scope: `ListRootsResult` and the four task results.
  *
  * There is no decoder for this union, and there cannot be one: nothing in a response says which
  * result it carries. A session layer that remembers what it asked picks the right member's decoder
  * itself. The encoder is well defined, because a value knows what it is.
  */
enum ClientResult:
  case Empty(result: EmptyResult)
  case CreateMessage(result: CreateMessageResult)
  case Elicit(result: ElicitResult)

object ClientResult:
  val encoder: Encoder[ClientResult] = value =>
    value match
      case ClientResult.Empty(result)         => EmptyResult.encoder.encode(result)
      case ClientResult.CreateMessage(result) => CreateMessageResult.encoder.encode(result)
      case ClientResult.Elicit(result)        => ElicitResult.encoder.encode(result)

/** A request a server sends (schema.ts:2534-2544).
  *
  * Omitted, being out of scope: `roots/list` and the four tasks requests (`tasks/get`,
  * `tasks/result`, `tasks/cancel`, `tasks/list`).
  */
enum ServerRequest:
  case Ping(params: Option[RequestParams])
  case CreateMessage(params: CreateMessageRequestParams)
  case Elicit(params: ElicitRequestParams)

  def method: String =
    this match
      case Ping(_)          => Method.Ping
      case CreateMessage(_) => Method.CreateMessage
      case Elicit(_)        => Method.Elicit

object ServerRequest:
  private[mcp] def read(fields: Fields, method: String): Either[DecodeError, ServerRequest] =
    method match
      case Method.Ping =>
        fields.opt("params", RequestParams.decoder).map(ServerRequest.Ping.apply)
      case Method.CreateMessage =>
        fields.req("params", CreateMessageRequestParams.decoder).map(ServerRequest.CreateMessage.apply)
      case Method.Elicit =>
        fields.req("params", ElicitRequestParams.decoder).map(ServerRequest.Elicit.apply)
      case other => Left(DecodeError.UnknownMethod(fields.path / "method", other))

  private[mcp] def write(request: ServerRequest, out: JsObj): Unit =
    request match
      case ServerRequest.Ping(params) =>
        out.putOpt("params", params, RequestParams.encoder)
      case ServerRequest.CreateMessage(params) =>
        out.put("params", params, CreateMessageRequestParams.encoder)
      case ServerRequest.Elicit(params) =>
        out.put("params", params, ElicitRequestParams.encoder)

/** A notification a server sends (schema.ts:2546-2556).
  *
  * Omitted, being out of scope: `notifications/resources/updated`,
  * `notifications/resources/list_changed`, `notifications/prompts/list_changed` and
  * `notifications/tasks/status`.
  */
enum ServerNotification:
  case Cancelled(params: CancelledNotificationParams)
  case Progress(params: ProgressNotificationParams)
  case LoggingMessage(params: LoggingMessageNotificationParams)
  case ToolListChanged(params: Option[NotificationParams])
  case ElicitationComplete(params: ElicitationCompleteNotificationParams)

  def method: String =
    this match
      case Cancelled(_)           => Method.Cancelled
      case Progress(_)            => Method.Progress
      case LoggingMessage(_)      => Method.LoggingMessage
      case ToolListChanged(_)     => Method.ToolListChanged
      case ElicitationComplete(_) => Method.ElicitationComplete

object ServerNotification:
  private[mcp] def read(fields: Fields, method: String): Either[DecodeError, ServerNotification] =
    method match
      case Method.Cancelled =>
        fields.req("params", CancelledNotificationParams.decoder).map(ServerNotification.Cancelled.apply)
      case Method.Progress =>
        fields.req("params", ProgressNotificationParams.decoder).map(ServerNotification.Progress.apply)
      case Method.LoggingMessage =>
        fields
          .req("params", LoggingMessageNotificationParams.decoder)
          .map(ServerNotification.LoggingMessage.apply)
      case Method.ToolListChanged =>
        fields.opt("params", NotificationParams.decoder).map(ServerNotification.ToolListChanged.apply)
      case Method.ElicitationComplete =>
        fields
          .req("params", ElicitationCompleteNotificationParams.decoder)
          .map(ServerNotification.ElicitationComplete.apply)
      case other => Left(DecodeError.UnknownMethod(fields.path / "method", other))

  private[mcp] def write(notification: ServerNotification, out: JsObj): Unit =
    notification match
      case ServerNotification.Cancelled(params) =>
        out.put("params", params, CancelledNotificationParams.encoder)
      case ServerNotification.Progress(params) =>
        out.put("params", params, ProgressNotificationParams.encoder)
      case ServerNotification.LoggingMessage(params) =>
        out.put("params", params, LoggingMessageNotificationParams.encoder)
      case ServerNotification.ToolListChanged(params) =>
        out.putOpt("params", params, NotificationParams.encoder)
      case ServerNotification.ElicitationComplete(params) =>
        out.put("params", params, ElicitationCompleteNotificationParams.encoder)

/** A result a server returns (schema.ts:2558-2573).
  *
  * Omitted, being out of scope: `CompleteResult`, `GetPromptResult`, `ListPromptsResult`,
  * `ListResourceTemplatesResult`, `ListResourcesResult`, `ReadResourceResult` and the four task
  * results. As with [[ClientResult]], there is no union decoder.
  */
enum ServerResult:
  case Empty(result: EmptyResult)
  case Initialize(result: InitializeResult)
  case CallTool(result: CallToolResult)
  case ListTools(result: ListToolsResult)

object ServerResult:
  val encoder: Encoder[ServerResult] = value =>
    value match
      case ServerResult.Empty(result)      => EmptyResult.encoder.encode(result)
      case ServerResult.Initialize(result) => InitializeResult.encoder.encode(result)
      case ServerResult.CallTool(result)   => CallToolResult.encoder.encode(result)
      case ServerResult.ListTools(result)  => ListToolsResult.encoder.encode(result)

/** The four envelope shapes, and the reading of them that both directions share. */
private object Envelope:
  /** Rejects a top-level array before anything else looks at it.
    *
    * JSON-RPC batching was removed from MCP in revision 2025-06-18 and 2025-11-25 does not bring it
    * back, so an array here is not a list of messages to process — it is a peer speaking a protocol
    * we do not.
    */
  def decoder[A](read: Fields => Either[DecodeError, A]): Decoder[A] = (at, value) =>
    if js.Array.isArray(value) then
      Left(DecodeError.Mismatch(at, "a single JSON-RPC message", "array"))
    else Decode.obj(read).decode(at, value)

  /** Which of the four shapes this object is, by which member it carries.
    *
    * `method` decides first: a request and a notification differ only by whether an `id` came with
    * it. An `id` spelled `null` counts as absent — the same leniency optional fields get everywhere
    * — so such a message reads as a notification.
    */
  def route[A](
    fields: Fields,
    request: (RequestId, String) => Either[DecodeError, A],
    notification: String => Either[DecodeError, A],
    response: RequestId => Either[DecodeError, A],
    error: (Option[RequestId], JsonRpc.Error) => Either[DecodeError, A]
  ): Either[DecodeError, A] =
    fields.req("jsonrpc", Decode.literal(JsonRpc.Version)).flatMap: _ =>
      if fields.present("method") then
        fields.req("method", Decode.string).flatMap: method =>
          if fields.present("id") then
            fields.req("id", RequestId.decoder).flatMap(request(_, method))
          else notification(method)
      else if fields.present("result") then fields.req("id", RequestId.decoder).flatMap(response)
      else if fields.present("error") then
        for
          id <- fields.opt("id", RequestId.decoder)
          failure <- fields.req("error", JsonRpc.Error.decoder)
          routed <- error(id, failure)
        yield routed
      else
        Left(
          DecodeError.Mismatch(
            fields.path,
            "a \"method\", \"result\" or \"error\" member",
            "object"
          )
        )

  def write(out: JsObj, id: Option[RequestId]): Unit =
    out.put("jsonrpc", JsonRpc.Version, Encode.string)
    out.putOpt("id", id, RequestId.encoder)

  /** The error envelope, whose absent id is written as an explicit `null` (ruling M12).
    *
    * This is the one place in the package where an encoder emits `null` rather than omitting the
    * member, and it is deliberate. The schema types the field as `id?: RequestId`, but that is a
    * loose TypeScript encoding of JSON-RPC 2.0 §5, whose normative text is not optional: an error
    * response whose request could not be identified MUST carry `"id": null`. A peer correlating
    * responses by id needs to be able to tell "this answers nothing" from "this member went
    * missing", and only the explicit null says the first.
    *
    * Reading is unchanged and lenient in both directions: an absent id and a null id both decode to
    * `None`, so this stays a round trip.
    */
  def writeError(out: JsObj, id: Option[RequestId]): Unit =
    out.put("jsonrpc", JsonRpc.Version, Encode.string)
    id match
      case Some(known) => out.put("id", known, RequestId.encoder)
      case None        => out.set("id", null)

/** Anything a client can put on the wire.
  *
  * [[Response]] and [[Error]] are the client's answers to requests the *server* made, so a
  * [[Response]] here carries a [[ClientResult]] payload once a session layer has typed it.
  */
enum ClientMessage:
  case Request(id: RequestId, request: ClientRequest)
  case Notification(notification: ClientNotification)
  case Response(id: RequestId, result: Json.Obj)
  case Error(id: Option[RequestId], error: JsonRpc.Error)

object ClientMessage:
  /** A typed result, packed into a response. */
  def response(id: RequestId, result: ClientResult): ClientMessage =
    ClientMessage.Response(id, Json.objFromJs(ClientResult.encoder.encode(result)))

  val decoder: Decoder[ClientMessage] = Envelope.decoder: fields =>
    Envelope.route(
      fields,
      (id, method) => ClientRequest.read(fields, method).map(ClientMessage.Request(id, _)),
      method => ClientNotification.read(fields, method).map(ClientMessage.Notification.apply),
      id => fields.req("result", Decode.jsonObj).map(ClientMessage.Response(id, _)),
      (id, error) => Right(ClientMessage.Error(id, error))
    )

  val encoder: Encoder[ClientMessage] = Encode.obj: (message, out) =>
    message match
      case ClientMessage.Request(id, request) =>
        Envelope.write(out, Some(id))
        out.put("method", request.method, Encode.string)
        ClientRequest.write(request, out)
      case ClientMessage.Notification(notification) =>
        Envelope.write(out, None)
        out.put("method", notification.method, Encode.string)
        ClientNotification.write(notification, out)
      case ClientMessage.Response(id, result) =>
        Envelope.write(out, Some(id))
        out.put("result", result, Encode.jsonObj)
      case ClientMessage.Error(id, error) =>
        Envelope.writeError(out, id)
        out.put("error", error, JsonRpc.Error.encoder)

/** Anything a server can put on the wire. */
enum ServerMessage:
  case Request(id: RequestId, request: ServerRequest)
  case Notification(notification: ServerNotification)
  case Response(id: RequestId, result: Json.Obj)
  case Error(id: Option[RequestId], error: JsonRpc.Error)

object ServerMessage:
  /** A typed result, packed into a response. */
  def response(id: RequestId, result: ServerResult): ServerMessage =
    ServerMessage.Response(id, Json.objFromJs(ServerResult.encoder.encode(result)))

  val decoder: Decoder[ServerMessage] = Envelope.decoder: fields =>
    Envelope.route(
      fields,
      (id, method) => ServerRequest.read(fields, method).map(ServerMessage.Request(id, _)),
      method => ServerNotification.read(fields, method).map(ServerMessage.Notification.apply),
      id => fields.req("result", Decode.jsonObj).map(ServerMessage.Response(id, _)),
      (id, error) => Right(ServerMessage.Error(id, error))
    )

  val encoder: Encoder[ServerMessage] = Encode.obj: (message, out) =>
    message match
      case ServerMessage.Request(id, request) =>
        Envelope.write(out, Some(id))
        out.put("method", request.method, Encode.string)
        ServerRequest.write(request, out)
      case ServerMessage.Notification(notification) =>
        Envelope.write(out, None)
        out.put("method", notification.method, Encode.string)
        ServerNotification.write(notification, out)
      case ServerMessage.Response(id, result) =>
        Envelope.write(out, Some(id))
        out.put("result", result, Encode.jsonObj)
      case ServerMessage.Error(id, error) =>
        Envelope.writeError(out, id)
        out.put("error", error, JsonRpc.Error.encoder)
