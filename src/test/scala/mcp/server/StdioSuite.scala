package mcp.server

import mcp.*

import scala.collection.mutable

/** The stdio transport with the process taken out (V4, V8).
  *
  * [[StdioPump]] is everything stdio decides; what is left in `Stdio.serve` is three lines of Node
  * event wiring that testing would only be testing Node. So the conversations here are real
  * conversations, driven by chunks arriving at awkward moments.
  */
class StdioSuite extends ServerSuite:

  /** A pump with its output collected, exactly as `Stdio.serve` builds one. */
  private final class Pipe:
    private val written = mutable.ArrayBuffer.empty[String]

    val pump: StdioPump = StdioPump(
      sink => ServerSession(Fixtures.serverInfo, Fixtures.capabilities, Some(EchoTools()), sink),
      text => written.append(text)
    )

    /** Everything written to stdout, in order, framing included. */
    def out: Vector[String] = written.toVector

    def clear(): Unit = written.clear()

  test("stdio: a request in one chunk is answered on one line"):
    val pipe = Pipe()
    pipe.pump.push(Fixtures.initializeRequest + "\n")
    assertEquals(pipe.out, Vector(Fixtures.initializeResult + "\n"))

  test("stdio: every line written ends with exactly one newline and holds none inside it"):
    val pipe = Pipe()
    pipe.pump.push(Fixtures.initializeRequest + "\n")
    pipe.pump.push(Fixtures.initializedNotification + "\n")
    pipe.pump.push(Fixtures.request(2, "tools/list") + "\n")
    for line <- pipe.out do
      assert(line.endsWith("\n"), line)
      assertEquals(line.count(_ == '\n'), 1, line)

  test("stdio: a request split across chunks is answered once it is whole"):
    val pipe = Pipe()
    val request = Fixtures.initializeRequest
    pipe.pump.push(request.substring(0, 20))
    assertEquals(pipe.out, Vector.empty[String])
    pipe.pump.push(request.substring(20))
    assertEquals(pipe.out, Vector.empty[String])
    pipe.pump.push("\n")
    assertEquals(pipe.out, Vector(Fixtures.initializeResult + "\n"))

  test("stdio: several messages in one chunk are answered in order"):
    val pipe = Pipe()
    pipe.pump.push(
      Fixtures.initializeRequest + "\n" + Fixtures.initializedNotification + "\n" +
        Fixtures.request(2, "tools/list") + "\n"
    )
    assertEquals(
      pipe.out,
      Vector(
        Fixtures.initializeResult + "\n",
        s"""{"jsonrpc":"2.0","id":2,"result":{"tools":[${Fixtures.echoToolJson}]}}""" + "\n"
      )
    )

  test("stdio: a notification produces no line at all"):
    val pipe = Pipe()
    pipe.pump.push(Fixtures.initializeRequest + "\n")
    pipe.clear()
    pipe.pump.push(Fixtures.initializedNotification + "\n")
    assertEquals(pipe.out, Vector.empty[String])

  test("stdio: sink traffic and direct replies share the one pipe"):
    // The distinction that matters over HTTP does not matter here; both are just lines.
    val pipe = Pipe()
    pipe.pump.push(Fixtures.initializeRequest + "\n" + Fixtures.initializedNotification + "\n")
    pipe.clear()
    pipe.pump.push(
      Fixtures.request(
        3,
        "tools/call",
        Some("""{"_meta":{"progressToken":"p"},"name":"echo","arguments":{"value":"x","notify":true}}""")
      ) + "\n"
    )
    assertEquals(pipe.out.length, 3)
    assert(pipe.out(0).contains("notifications/progress"), pipe.out(0))
    assert(pipe.out(1).contains("notifications/message"), pipe.out(1))
    assert(pipe.out(2).contains("\"result\""), pipe.out(2))

  test("stdio: a server-initiated request is framed like everything else"):
    val pipe = Pipe()
    pipe.pump.session.ping(_ => ())
    assertEquals(pipe.out, Vector("""{"jsonrpc":"2.0","id":1,"method":"ping"}""" + "\n"))

  test("stdio: a response arriving on stdin settles the pending call"):
    val pipe = Pipe()
    var answered = false
    pipe.pump.session.ping(_ => answered = true)
    pipe.clear()
    pipe.pump.push("""{"jsonrpc":"2.0","id":1,"result":{}}""" + "\n")
    assert(answered)
    assertEquals(pipe.out, Vector.empty[String])

  test("stdio: blank lines between messages are ignored"):
    val pipe = Pipe()
    pipe.pump.push("\n\n" + Fixtures.initializeRequest + "\n\n")
    assertEquals(pipe.out, Vector(Fixtures.initializeResult + "\n"))

  test("stdio: CRLF framing works end to end"):
    val pipe = Pipe()
    pipe.pump.push(Fixtures.initializeRequest + "\r\n")
    assertEquals(pipe.out, Vector(Fixtures.initializeResult + "\n"))

  test("stdio: a stream that ends without a final newline still delivers its last message"):
    val pipe = Pipe()
    pipe.pump.push(Fixtures.initializeRequest)
    assertEquals(pipe.out, Vector.empty[String])
    pipe.pump.end()
    assertEquals(pipe.out, Vector(Fixtures.initializeResult + "\n"))

  test("stdio: ending a stream that owes nothing writes nothing"):
    val pipe = Pipe()
    pipe.pump.push(Fixtures.initializeRequest + "\n")
    pipe.clear()
    pipe.pump.end()
    assertEquals(pipe.out, Vector.empty[String])

  test("stdio: garbage on stdin is answered rather than crashing the pump"):
    val pipe = Pipe()
    pipe.pump.push("not json at all\n")
    assertEquals(pipe.out.length, 1)
    assert(pipe.out.head.contains("-32700"), pipe.out.head)
    // And the pump carries on with the next message.
    pipe.clear()
    pipe.pump.push(Fixtures.initializeRequest + "\n")
    assertEquals(pipe.out, Vector(Fixtures.initializeResult + "\n"))

  test("stdio: everything written is a decodable server message"):
    val pipe = Pipe()
    pipe.pump.push(Fixtures.initializeRequest + "\n" + Fixtures.initializedNotification + "\n")
    pipe.pump.push(Fixtures.request(2, "tools/list") + "\n")
    pipe.pump.push("garbage\n")
    pipe.pump.session.ping(_ => ())
    for line <- pipe.out do
      assert(line.endsWith("\n"), line)
      assert(Codec.decodeServerMessage(line.dropRight(1)).isRight, line)
