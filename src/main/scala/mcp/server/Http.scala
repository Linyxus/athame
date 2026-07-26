package mcp.server

import mcp.*

import scala.collection.mutable
import scala.scalajs.js

/** The Streamable HTTP transport: one endpoint, three verbs, a session per `Mcp-Session-Id`.
  *
  * ==Why a POST is never answered with a stream==
  *
  * The spec permits answering a POST with either a single JSON response or an SSE stream. This
  * server always chooses the single response, which is the simpler half of a permitted choice, and
  * the consequence is worth stating plainly: a tool that reports progress mid-call does *not* get
  * those notifications interleaved ahead of its reply on the POST. They go to the session's GET
  * stream, and if none is open they wait in the queue until one is. A client that wants live
  * progress has to open the GET stream; a client that only wants answers can ignore it entirely.
  *
  * That split is exactly what [[ServerSession.receive]] returning its replies buys: the reply rides
  * the response that carried its request, while sink traffic goes somewhere else.
  *
  * ==Security==
  *
  * The boundary is the loopback bind, per the transport spec's own guidance: no TLS, no
  * authentication, and [[HttpServer.localhostOrigin]] refuses any `Origin` that is not localhost,
  * which is what stops a page in a browser from talking to a server running on the user's machine.
  * A deployment that binds anywhere else needs both of the things this does not have.
  */
object HttpServer:

  /** The default origin policy: `http` or `https`, host `localhost` or `127.0.0.1`, any port.
    *
    * A request with no `Origin` at all is allowed — that is a non-browser client, which the header
    * exists to distinguish from a browser one.
    */
  def localhostOrigin(origin: String): Boolean =
    val rest =
      if origin.startsWith("http://") then origin.drop("http://".length)
      else if origin.startsWith("https://") then origin.drop("https://".length)
      else ""
    val host = rest.takeWhile(_ != ':')
    val port = rest.drop(host.length)
    (host == "localhost" || host == "127.0.0.1") &&
    (port.isEmpty || (port.startsWith(":") && port.drop(1).forall(_.isDigit) && port.length > 1))

/** @param build
  *   makes a session from the sink that reaches that session's client; called once per session.
  * @param host
  *   loopback by default, which is the security boundary rather than a convenience.
  * @param port
  *   0 asks the OS for an ephemeral one; [[boundPort]] reports what it gave.
  * @param newSessionId
  *   injectable so that tests can pin ids; `crypto.randomUUID` in production.
  */
final class HttpServer(
  build: (String => Unit) => ServerSession,
  host: String = "127.0.0.1",
  port: Int = 0,
  endpoint: String = "/mcp",
  originAllowed: String => Boolean = HttpServer.localhostOrigin,
  newSessionId: () => String = () => NodeCrypto.randomUUID()
):

  /** One client's session, plus wherever its server-initiated traffic is currently going. */
  private final class Connection(val id: String):
    private val queued = mutable.ArrayBuffer.empty[String]
    private var stream: Option[ServerResponse] = None

    val session: ServerSession = build(emit)

    /** Unbounded in v1: a client that never opens a stream and never deletes its session will
      * accumulate. Bounding it means deciding what to drop, and dropping protocol messages is
      * worse than the leak until there is a reason to think otherwise.
      */
    private def emit(text: String): Unit =
      stream match
        case Some(response) => write(response, text)
        case None           => queued.append(text)

    private def write(response: ServerResponse, text: String): Unit =
      // No `id:` line: resumability is out of scope, and advertising an id we would not honour on
      // Last-Event-ID would be worse than not offering it.
      response.write(s"data: $text\n\n")

    def streaming: Boolean = stream.isDefined

    def queueDepth: Int = queued.length

    def open(response: ServerResponse): Unit =
      stream = Some(response)
      queued.foreach(text => write(response, text))
      queued.clear()

    /** The client hung up. The response is already finished, so it must not be ended again. */
    def detach(): Unit = stream = None

    def close(): Unit =
      stream.foreach(_.end())
      stream = None

  private enum Lookup:
    case Found(connection: Connection)
    case Missing
    case Unknown

  private val sessions = mutable.Map.empty[String, Connection]
  private var listener: Option[HttpListener] = None
  private var bound: Int = 0

  /** The port actually listening, which is only interesting when [[port]] was 0. */
  def boundPort: Int = bound

  def sessionCount: Int = sessions.size

  def start(onListening: Int => Unit): Unit =
    val server = NodeHttp.createServer((request, response) => accept(request, response))
    listener = Some(server)
    server.listen(
      port,
      host,
      () =>
        bound = server.address().port.asInstanceOf[Int]
        onListening(bound)
    )

  /** Ends every open stream first: Node's `close` waits for connections to drain, and an SSE
    * stream never drains on its own.
    */
  def stop(onClosed: () => Unit): Unit =
    sessions.values.foreach(_.close())
    sessions.clear()
    listener match
      case Some(server) => server.close(() => onClosed())
      case None         => onClosed()

  // -----------------------------------------------------------------------------------------
  // Routing
  // -----------------------------------------------------------------------------------------

  private def accept(request: IncomingMessage, response: ServerResponse): Unit =
    request.setEncoding("utf8")
    val body = StringBuilder()
    request.on("data", chunk => body.append(chunk.asInstanceOf[String]))
    request.on("end", _ => route(request, response, body.toString))

  private def route(request: IncomingMessage, response: ServerResponse, body: String): Unit =
    if path(request) != endpoint then status(response, 404)
    else
      header(request, "origin") match
        case Some(origin) if !originAllowed(origin) => status(response, 403)
        case _ =>
          request.method match
            // POST checks the version itself, because the initialize POST is exempt from it and
            // only the body says whether this is that one.
            case "POST"   => post(request, response, body)
            case "GET"    => withVersion(request, response)(get(request, response))
            case "DELETE" => withVersion(request, response)(delete(request, response))
            case _        => status(response, 405)

  private def post(request: IncomingMessage, response: ServerResponse, body: String): Unit =
    if opensSession(body) then
      val id = newSessionId()
      val connection = Connection(id)
      sessions(id) = connection
      answer(response, connection.session.receive(body), Seq("mcp-session-id" -> id))
    else if !versionAcceptable(request) then status(response, 400)
    else
      withSession(request, response): connection =>
        // Runs even when the answer will be 202: a response to something we asked has to reach the
        // pending table whether or not anything goes back on this request.
        answer(response, connection.session.receive(body), Seq.empty)

  /** Whether the reply the session produced belongs on this response.
    *
    * A request earns exactly one reply and gets 200; a notification or a response earns none and
    * gets 202 with an empty body. Deciding it this way rather than by re-classifying the message
    * means the two can never disagree about what kind of message it was.
    */
  private def answer(
    response: ServerResponse,
    replies: Vector[String],
    extra: Seq[(String, String)]
  ): Unit =
    replies.headOption match
      case Some(reply) => json(response, 200, reply, extra)
      case None        => status(response, 202, extra)

  private def get(request: IncomingMessage, response: ServerResponse): Unit =
    // Checked before the session, in the order the spec words the two rules.
    if !acceptsEvents(request) then status(response, 405)
    else
      withSession(request, response): connection =>
        if connection.streaming then status(response, 409)
        else
          response.writeHead(
            200,
            js.Dictionary(
              "content-type" -> "text/event-stream",
              "cache-control" -> "no-cache",
              "connection" -> "keep-alive"
            )
          )
          // Before anything is queued or written: a client waiting on this GET learns the stream
          // is open from the headers, and a session with nothing to say yet would otherwise leave
          // it waiting indefinitely for a response that had, in fact, already started.
          response.flushHeaders()
          connection.open(response)
          // The response's close, not the request's: a GET's request completes the moment its
          // (absent) body has been read, which is not the client going away.
          response.on("close", _ => connection.detach())

  private def delete(request: IncomingMessage, response: ServerResponse): Unit =
    withSession(request, response): connection =>
      connection.close()
      sessions.remove(connection.id)
      status(response, 204)

  /** The session named by the header, or the status that says why there is none. */
  private def withSession(request: IncomingMessage, response: ServerResponse)(
    handle: Connection => Unit
  ): Unit =
    lookup(request) match
      case Lookup.Found(connection) => handle(connection)
      case Lookup.Missing           => status(response, 400)
      case Lookup.Unknown           => status(response, 404)

  private def lookup(request: IncomingMessage): Lookup =
    header(request, "mcp-session-id") match
      case None     => Lookup.Missing
      case Some(id) => sessions.get(id).map(Lookup.Found.apply).getOrElse(Lookup.Unknown)

  // -----------------------------------------------------------------------------------------
  // Reading the request
  // -----------------------------------------------------------------------------------------

  private def path(request: IncomingMessage): String = request.url.takeWhile(_ != '?')

  private def header(request: IncomingMessage, name: String): Option[String] =
    request.headers
      .get(name)
      .filter(value => value != null && !js.isUndefined(value))
      .map(_.toString)

  /** Whether this body is an `initialize` request, and so mints a session rather than needing one.
    *
    * This decodes the message a second time — the session will decode it again itself. The
    * alternative is handing the session a pre-decoded message, which would mean a second entry
    * point beside `receive(text)` and two ways for a transport to drive a session. One extra parse
    * on the one message per session that takes this path is the cheaper trade.
    */
  private def opensSession(body: String): Boolean =
    Codec.decodeClientMessage(body) match
      case Right(ClientMessage.Request(_, ClientRequest.Initialize(_))) => true
      case _                                                            => false

  /** Checked on every request but one: POST, GET and DELETE alike.
    *
    * The exception is the `initialize` POST, which is exempt because that exchange is where a
    * client finds out which revision it is talking to — requiring the header there would oblige it
    * to have known the answer before it asked the question.
    *
    * An absent header is accepted everywhere. Sending it is the client's obligation, and a server
    * that refused requests for the omission would break clients over a header that tells it
    * nothing it does not already know from the handshake.
    */
  private def versionAcceptable(request: IncomingMessage): Boolean =
    header(request, "mcp-protocol-version").forall(_ == Mcp.LatestProtocolVersion)

  private def withVersion(request: IncomingMessage, response: ServerResponse)(
    handle: => Unit
  ): Unit =
    if versionAcceptable(request) then handle else status(response, 400)

  /** The spec obliges clients to offer both content types on a POST; v1 does not enforce that, and
    * only asks about `Accept` on a GET, where the answer decides whether a stream is even possible.
    */
  private def acceptsEvents(request: IncomingMessage): Boolean =
    header(request, "accept").exists(_.contains("text/event-stream"))

  // -----------------------------------------------------------------------------------------
  // Writing the response
  // -----------------------------------------------------------------------------------------

  /** Every status that is not a JSON reply answers with the status alone. There is no error body
    * to get out of step with the code, and 202 and 204 are required to be empty anyway.
    */
  private def status(response: ServerResponse, code: Int, extra: Seq[(String, String)] = Seq.empty): Unit =
    response.writeHead(code, headers(extra))
    response.end()

  private def json(
    response: ServerResponse,
    code: Int,
    body: String,
    extra: Seq[(String, String)]
  ): Unit =
    response.writeHead(code, headers(("content-type" -> "application/json") +: extra))
    response.end(body)

  private def headers(entries: Seq[(String, String)]): js.Dictionary[String] =
    val out = js.Dictionary[String]()
    entries.foreach((name, value) => out(name) = value)
    out
