package mcp.server

import mcp.*

import scala.collection.mutable

/** Shared plumbing for the `mcp.server` suites. */
trait ServerSuite extends mcp.McpSuite:

  /** The one reply a request earns, or a failure saying what came back instead. */
  def single(replies: Vector[String])(using munit.Location): String =
    replies match
      case Vector(reply) => reply
      case other =>
        fail(s"expected exactly one reply, got ${other.length}: ${other.mkString(" | ")}")

  /** Asserts a request earns no reply at all — what a notification, or a response to something we
    * asked, is owed.
    */
  def assertSilent(replies: Vector[String])(using munit.Location): Unit =
    assertEquals(replies, Vector.empty[String], "expected no reply")

/** The demo tool.
  *
  * One tool exercises every facility a [[ToolContext]] offers and every way a call can end, chosen
  * by flags in its own arguments so that a test states its intent in the message it sends rather
  * than by swapping providers:
  *
  *   - by default, echoes `value` back as text;
  *   - `"fail": true` → a tool-execution failure, which is a *result* with `isError`, the thing a
  *     model is supposed to see and correct;
  *   - `"break": true` → the provider itself failing, a `Left`, which the model never sees;
  *   - `"throw": true` → the provider throwing, which the session has to survive;
  *   - `"notify": true` → a progress update and a log message before the reply.
  */
final class EchoTools extends ToolProvider:

  def list(): Vector[Tool] = Vector(Fixtures.echoTool)

  def call(
    name: String,
    arguments: Option[Json.Obj],
    context: ToolContext
  ): Either[String, CallToolResult] =
    if flag(arguments, "throw") then throw RuntimeException("kaboom")
    else if flag(arguments, "break") then Left("provider exploded")
    else
      if flag(arguments, "notify") then
        context.progress(1, Some(2), Some("halfway"))
        context.log(LoggingLevel.Info, Some("echo"), Json.Str("working"))
      val echoed = arguments.flatMap(_.get("value")) match
        case Some(Json.Str(text)) => text
        case Some(other)          => Json.stringify(other)
        case None                 => ""
      val failed = flag(arguments, "fail")
      val text = if failed then s"failed: $echoed" else echoed
      Right(
        CallToolResult(
          None,
          Vector(TextContent(text, None, None)),
          None,
          if failed then Some(true) else None
        )
      )

  private def flag(arguments: Option[Json.Obj], name: String): Boolean =
    arguments.flatMap(_.get(name)).contains(Json.Bool(true))

object Fixtures:

  val serverInfo: Implementation = Implementation("athame-test", None, None, "0.1.0", None, None)
  val clientInfo: Implementation = Implementation("test-client", None, None, "1.0.0", None, None)

  val echoSchema: JsonSchemaObject = JsonSchemaObject(
    None,
    Some(Json.obj("value" -> Json.obj("type" -> Json.Str("string")))),
    Some(Vector("value"))
  )

  val echoTool: Tool =
    Tool("echo", None, None, Some("Returns its argument"), echoSchema, None, None, None, None)

  /** Logging declared, tools left to the session to derive from the provider. */
  val capabilities: ServerCapabilities = ServerCapabilities.empty.copy(logging = Some(Json.obj()))

  // --- pinned wire vectors --------------------------------------------------------------------

  val initializeRequest: String =
    """{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2025-11-25","capabilities":{},"clientInfo":{"name":"test-client","version":"1.0.0"}}}"""

  val initializeResult: String =
    """{"jsonrpc":"2.0","id":1,"result":{"protocolVersion":"2025-11-25","capabilities":{"logging":{},"tools":{}},"serverInfo":{"name":"athame-test","version":"0.1.0"}}}"""

  val initializedNotification: String =
    """{"jsonrpc":"2.0","method":"notifications/initialized"}"""

  val echoToolJson: String =
    """{"name":"echo","description":"Returns its argument","inputSchema":{"type":"object","properties":{"value":{"type":"string"}},"required":["value"]}}"""

  def request(id: Double, method: String, params: Option[String] = None): String =
    val tail = params.map(text => s""","params":$text""").getOrElse("")
    s"""{"jsonrpc":"2.0","id":${Codec.show(id)},"method":"$method"$tail}"""

/** A session with its sink hooked to a buffer.
  *
  * The sink is the only thing a test cannot read off a return value, so it gets a first-class
  * accessor; the direct replies come back from [[receive]] the way the transport sees them.
  */
final class Harness(
  tools: Option[ToolProvider] = Some(EchoTools()),
  capabilities: ServerCapabilities = Fixtures.capabilities,
  instructions: Option[String] = None
):

  private val collected = mutable.ArrayBuffer.empty[String]

  val session: ServerSession =
    ServerSession(
      Fixtures.serverInfo,
      capabilities,
      tools,
      text => collected.append(text),
      instructions
    )

  def receive(text: String): Vector[String] = session.receive(text)

  /** Everything the server sent on its own initiative, oldest first. */
  def sink: Vector[String] = collected.toVector

  def clearSink(): Unit = collected.clear()

  /** Drive the handshake to [[SessionState.Ready]] and return this, for chaining. */
  def ready(): Harness =
    receive(Fixtures.initializeRequest)
    receive(Fixtures.initializedNotification)
    this
