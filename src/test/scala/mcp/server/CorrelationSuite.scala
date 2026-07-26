package mcp.server

import mcp.*

import scala.collection.mutable

/** Server→client requests and the answers to them (V2, V8).
  *
  * The plumbing sampling and elicitation will sit on, exercised here by `ping` because `ping` is
  * the one server-initiated request this increment ships. Ids, the pending table and the callback
  * contract are the parts that have to be right before anything typed is built on top.
  */
class CorrelationSuite extends ServerSuite:

  /** Records what a callback was handed, so a test can assert on it after the fact. */
  private final class Outcomes[A]:
    private val seen = mutable.ArrayBuffer.empty[Either[CallFailure, A]]
    val record: Either[CallFailure, A] => Unit = outcome => seen.append(outcome)
    def all: Vector[Either[CallFailure, A]] = seen.toVector
    def only(using munit.Location): Either[CallFailure, A] =
      seen.toVector match
        case Vector(one) => one
        case other       => fail(s"expected one outcome, got ${other.length}")

  test("correlation: a server request goes out through the sink, not as a reply"):
    val harness = Harness().ready()
    harness.session.ping(_ => ())
    assertEquals(harness.sink, Vector("""{"jsonrpc":"2.0","id":1,"method":"ping"}"""))

  test("correlation: ids come from the session's own counter, in order"):
    val harness = Harness().ready()
    harness.session.ping(_ => ())
    harness.session.ping(_ => ())
    harness.session.ping(_ => ())
    assertEquals(
      harness.sink,
      Vector(
        """{"jsonrpc":"2.0","id":1,"method":"ping"}""",
        """{"jsonrpc":"2.0","id":2,"method":"ping"}""",
        """{"jsonrpc":"2.0","id":3,"method":"ping"}"""
      )
    )

  test("correlation: the client's ids and the server's never get confused for one another"):
    // JSON-RPC gives each direction its own id space. The client here uses id 1 for its own
    // request at the same time the server is waiting on its own id 1.
    val harness = Harness().ready()
    harness.session.ping(_ => ())
    assertEquals(
      single(harness.receive(Fixtures.request(1, "tools/list"))),
      s"""{"jsonrpc":"2.0","id":1,"result":{"tools":[${Fixtures.echoToolJson}]}}"""
    )
    assertEquals(harness.session.pendingRequests, 1)

  test("correlation: a result routes to the callback and clears the pending entry"):
    val harness = Harness().ready()
    val outcomes = Outcomes[Unit]()
    harness.session.ping(outcomes.record)
    assertEquals(harness.session.pendingRequests, 1)
    assertSilent(harness.receive("""{"jsonrpc":"2.0","id":1,"result":{}}"""))
    assertEquals(outcomes.only, Right(()))
    assertEquals(harness.session.pendingRequests, 0)

  test("correlation: the callback runs inside the receive that carried the answer"):
    // Synchronous, so a transport can rely on everything a callback does having happened by the
    // time receive returns.
    val harness = Harness().ready()
    var ranDuring = false
    var returned = false
    harness.session.ping(_ => ranDuring = !returned)
    harness.receive("""{"jsonrpc":"2.0","id":1,"result":{}}""")
    returned = true
    assert(ranDuring)

  test("correlation: an error response arrives as a Left carrying the peer's error"):
    val harness = Harness().ready()
    val outcomes = Outcomes[Unit]()
    harness.session.ping(outcomes.record)
    assertSilent(
      harness.receive("""{"jsonrpc":"2.0","id":1,"error":{"code":-32601,"message":"no ping here"}}""")
    )
    assertEquals(
      outcomes.only,
      Left(CallFailure.Returned(JsonRpc.Error(ErrorCode.MethodNotFound, "no ping here", None)))
    )
    assertEquals(outcomes.only.left.map(_.message), Left("no ping here"))
    assertEquals(harness.session.pendingRequests, 0)

  test("correlation: a result that is not the shape we asked for is a Left, not a throw"):
    val harness = Harness().ready()
    val outcomes = Outcomes[InitializeResult]()
    harness.session.request(ServerRequest.Ping(None), InitializeResult.decoder)(outcomes.record)
    assertSilent(harness.receive("""{"jsonrpc":"2.0","id":1,"result":{}}"""))
    assertEquals(
      outcomes.only,
      Left(CallFailure.Undecodable(DecodeError.Missing(Path.root / "protocolVersion")))
    )
    assertEquals(
      outcomes.only.left.map(_.message),
      Left("$.protocolVersion: missing required field")
    )

  test("correlation: a typed request decodes a real result into a value"):
    val harness = Harness().ready()
    val outcomes = Outcomes[EmptyResult]()
    harness.session.request(ServerRequest.Ping(None), EmptyResult.decoder)(outcomes.record)
    harness.receive(s"""{"jsonrpc":"2.0","id":1,"result":{"_meta":{"a":1}}}""")
    assertEquals(outcomes.only, Right(EmptyResult(Some(Json.obj("a" -> Json.Num(1))))))

  test("correlation: a response to an id we never sent is dropped, silently"):
    val harness = Harness().ready()
    val outcomes = Outcomes[Unit]()
    harness.session.ping(outcomes.record)
    assertSilent(harness.receive("""{"jsonrpc":"2.0","id":99,"result":{}}"""))
    assertEquals(outcomes.all, Vector.empty)
    assertEquals(harness.session.pendingRequests, 1)

  test("correlation: answering the same id twice only settles it once"):
    val harness = Harness().ready()
    val outcomes = Outcomes[Unit]()
    harness.session.ping(outcomes.record)
    harness.receive("""{"jsonrpc":"2.0","id":1,"result":{}}""")
    harness.receive("""{"jsonrpc":"2.0","id":1,"result":{}}""")
    assertEquals(outcomes.all.length, 1)

  test("correlation: an error response with no id has nobody to route to"):
    val harness = Harness().ready()
    val outcomes = Outcomes[Unit]()
    harness.session.ping(outcomes.record)
    assertSilent(harness.receive("""{"jsonrpc":"2.0","error":{"code":-32700,"message":"Parse error"}}"""))
    assertEquals(outcomes.all, Vector.empty)
    assertEquals(harness.session.pendingRequests, 1)

  test("correlation: several requests in flight settle independently, in any order"):
    val harness = Harness().ready()
    val first = Outcomes[Unit]()
    val second = Outcomes[Unit]()
    harness.session.ping(first.record)
    harness.session.ping(second.record)
    assertEquals(harness.session.pendingRequests, 2)
    harness.receive("""{"jsonrpc":"2.0","id":2,"result":{}}""")
    assertEquals(second.only, Right(()))
    assertEquals(first.all, Vector.empty)
    harness.receive("""{"jsonrpc":"2.0","id":1,"result":{}}""")
    assertEquals(first.only, Right(()))
    assertEquals(harness.session.pendingRequests, 0)

  test("correlation: a server may ping before the handshake has finished"):
    // Nothing in the protocol gates the server's own requests on the client's initialize.
    val harness = Harness()
    harness.session.ping(_ => ())
    assertEquals(harness.sink, Vector("""{"jsonrpc":"2.0","id":1,"method":"ping"}"""))

  test("correlation: a full ping round trip leaves the session as it found it"):
    val harness = Harness().ready()
    val outcomes = Outcomes[Unit]()
    harness.session.ping(outcomes.record)
    val asked = harness.sink.head
    assertEquals(Codec.decodeServerMessage(asked), Right(ServerMessage.Request(RequestId.Num(1), ServerRequest.Ping(None))))
    assertSilent(harness.receive("""{"jsonrpc":"2.0","id":1,"result":{}}"""))
    assertEquals(outcomes.only, Right(()))
    assertEquals(harness.session.state, SessionState.Ready)
    assertEquals(harness.session.pendingRequests, 0)
