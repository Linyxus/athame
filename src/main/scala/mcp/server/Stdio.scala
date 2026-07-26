package mcp.server

/** The stdio transport: newline-delimited JSON-RPC over a process's own pipes.
  *
  * One message per line, in both directions. `JSON.stringify` never emits a raw newline — it
  * escapes them inside strings — so an encoded message is always exactly one line, and framing
  * needs nothing cleverer than a `\n`.
  *
  * stdout carries MCP messages and nothing else. A server that prints a banner, a warning or a
  * stray `println` to stdout has corrupted the stream and its client will say so; stderr is free
  * for all of that.
  */

/** The transport with the process taken out: chunks in, framed lines out.
  *
  * The session is built here rather than passed in, because the sink it needs is this class's
  * `emit` — the two are mutually dependent and something has to close the loop. `build` receives
  * that sink and returns the session wired to it.
  *
  * Everything the stdio transport actually decides lives here, which is what lets the suite drive
  * a whole conversation with a collecting function and no process at all.
  */
final class StdioPump(build: (String => Unit) => ServerSession, out: String => Unit):

  private val frames = LineFrames()

  /** Both direct replies and sink traffic leave the same way — stdio has one pipe, so the
    * distinction that matters to HTTP does not matter here.
    */
  private def emit(text: String): Unit = out(text + "\n")

  private val target: ServerSession = build(emit)

  /** The session being driven, for callers that want to `log` or `ping` through it. */
  def session: ServerSession = target

  /** Feed a chunk of stdin. Any complete messages in it are handled and answered. */
  def push(chunk: String): Unit = frames.push(chunk).foreach(handle)

  /** Signal end of stream, handling a final unterminated line if the peer left one. */
  def end(): Unit = frames.flush().foreach(handle)

  private def handle(line: String): Unit = target.receive(line).foreach(emit)

/** Wiring [[StdioPump]] to the real process. Thin on purpose: there is nothing here to test that
  * testing Node would not be testing.
  */
object Stdio:

  def serve(build: (String => Unit) => ServerSession): StdioPump =
    val pump = StdioPump(build, text => NodeProcess.stdout.write(text))
    NodeProcess.stdin.setEncoding("utf8")
    NodeProcess.stdin.on("data", chunk => pump.push(chunk.asInstanceOf[String]))
    NodeProcess.stdin.on("end", _ => pump.end())
    pump
