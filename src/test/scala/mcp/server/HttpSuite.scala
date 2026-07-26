package mcp.server

import mcp.*

import scala.collection.mutable
import scala.concurrent.{Future, Promise}

/** The Streamable HTTP transport, against a real Node server on an ephemeral port (V5, V6, V8).
  *
  * Nothing here sleeps. Every wait is a promise completed by the thing being waited for — a
  * response arriving, an SSE event landing — so the suite is as fast as the loopback interface and
  * cannot flake on a slow machine.
  */
class HttpSuite extends ServerSuite:

  import scala.concurrent.ExecutionContext.Implicits.global

  /** Hands the test the sessions the server built, which is otherwise private to it. */
  private final class Sessions:
    private val made = mutable.ArrayBuffer.empty[ServerSession]

    val build: (String => Unit) => ServerSession = sink =>
      val session =
        ServerSession(Fixtures.serverInfo, Fixtures.capabilities, Some(EchoTools()), sink)
      made.append(session)
      session

    def first: ServerSession = made.head
    def count: Int = made.length

  /** A server on port 0, stopped however the body ends. */
  private def withServer(body: (HttpClient, HttpServer, Sessions) => Future[Any]): Future[Any] =
    val sessions = Sessions()
    var counter = 0
    val server = HttpServer(
      sessions.build,
      port = 0,
      newSessionId = () =>
        counter += 1
        s"session-$counter"
    )
    val started = Promise[Int]()
    server.start(port => started.success(port))
    started.future.flatMap: port =>
      body(HttpClient(port), server, sessions).transformWith: outcome =>
        val stopped = Promise[Unit]()
        server.stop(() => stopped.success(()))
        stopped.future.transform(_ => outcome)

  private val sse = Map("accept" -> "text/event-stream")

  private def opened(client: HttpClient, id: String): Future[Reply] =
    client.post(Fixtures.initializeRequest).map: reply =>
      assertEquals(reply.status, 200)
      assertEquals(reply.sessionId, Some(id))
      reply

  private def idHeader(id: String): Map[String, String] = Map("mcp-session-id" -> id)

  // --- initialize and session minting ---------------------------------------------------------

  test("http: an initialize POST mints a session and answers on the response"):
    withServer: (client, _, _) =>
      client.post(Fixtures.initializeRequest).map: reply =>
        assertEquals(reply.status, 200)
        assertEquals(reply.sessionId, Some("session-1"))
        assertEquals(reply.contentType, Some("application/json"))
        assertEquals(reply.body, Fixtures.initializeResult)

  test("http: each initialize POST mints its own session"):
    withServer: (client, server, sessions) =>
      for
        first <- client.post(Fixtures.initializeRequest)
        second <- client.post(Fixtures.initializeRequest)
      yield
        assertEquals(first.sessionId, Some("session-1"))
        assertEquals(second.sessionId, Some("session-2"))
        assertEquals(server.sessionCount, 2)
        assertEquals(sessions.count, 2)

  test("http: a session id on an initialize POST is ignored, not refused"):
    // V6 does not say; minting a fresh session is friendlier than rejecting a client that kept a
    // stale header around.
    withServer: (client, _, _) =>
      client.post(Fixtures.initializeRequest, idHeader("session-99")).map: reply =>
        assertEquals(reply.status, 200)
        assertEquals(reply.sessionId, Some("session-1"))

  // --- the status matrix ------------------------------------------------------------------------

  test("http: a path that is not the endpoint is not found"):
    withServer: (client, _, _) =>
      client.send("POST", Some(Fixtures.initializeRequest), Map.empty, at = "/elsewhere").map: reply =>
        assertEquals(reply.status, 404)

  test("http: an origin that is not localhost is refused before the body is looked at"):
    withServer: (client, server, _) =>
      client.post(Fixtures.initializeRequest, Map("origin" -> "https://evil.test")).map: reply =>
        assertEquals(reply.status, 403)
        assertEquals(reply.body, "")
        // No session was minted, so the body really was not processed.
        assertEquals(server.sessionCount, 0)

  test("http: localhost origins are allowed, in both schemes and on any port"):
    withServer: (client, _, _) =>
      val origins = Vector("http://localhost:3000", "https://localhost", "http://127.0.0.1:8080")
      Future
        .sequence(origins.map(origin => client.post(Fixtures.initializeRequest, Map("origin" -> origin))))
        .map(replies => assertEquals(replies.map(_.status), origins.map(_ => 200)))

  test("http: a request with no origin at all is allowed, being a non-browser client"):
    withServer: (client, _, _) =>
      client.post(Fixtures.initializeRequest).map(reply => assertEquals(reply.status, 200))

  test("http: a non-initialize POST with no session header is a bad request"):
    withServer: (client, _, _) =>
      client.post(Fixtures.request(2, "tools/list")).map: reply =>
        assertEquals(reply.status, 400)
        assertEquals(reply.body, "")

  test("http: a POST naming a session we do not have is not found"):
    withServer: (client, _, _) =>
      client.post(Fixtures.request(2, "tools/list"), idHeader("session-nope")).map: reply =>
        assertEquals(reply.status, 404)

  test("http: a wrong protocol-version header is a bad request"):
    withServer: (client, _, _) =>
      for
        _ <- opened(client, "session-1")
        reply <- client.post(
          Fixtures.request(2, "tools/list"),
          idHeader("session-1") + ("mcp-protocol-version" -> "2024-11-05")
        )
      yield assertEquals(reply.status, 400)

  test("http: the right protocol-version header is accepted, and so is none at all"):
    withServer: (client, _, _) =>
      for
        _ <- opened(client, "session-1")
        _ <- client.post(Fixtures.initializedNotification, idHeader("session-1"))
        stated <- client.post(
          Fixtures.request(2, "tools/list"),
          idHeader("session-1") + ("mcp-protocol-version" -> "2025-11-25")
        )
        silent <- client.post(Fixtures.request(3, "tools/list"), idHeader("session-1"))
      yield
        assertEquals(stated.status, 200)
        assertEquals(silent.status, 200)

  test("http: the version header is not required on the initialize POST itself"):
    // That exchange is where a client finds out which revision it is talking to.
    withServer: (client, _, _) =>
      client.post(Fixtures.initializeRequest).map(reply => assertEquals(reply.status, 200))

  test("http: the version header is checked on GET and DELETE too, not only on POST"):
    withServer: (client, _, _) =>
      val stale = Map("mcp-protocol-version" -> "2024-11-05")
      for
        _ <- opened(client, "session-1")
        getReply <- client.send("GET", None, sse ++ idHeader("session-1") ++ stale)
        deleteReply <- client.delete(idHeader("session-1") ++ stale)
      yield
        assertEquals(getReply.status, 400)
        assertEquals(deleteReply.status, 400)

  test("http: the right version header is fine on GET and DELETE, and so is none"):
    withServer: (client, _, _) =>
      val current = Map("mcp-protocol-version" -> "2025-11-25")
      for
        _ <- opened(client, "session-1")
        stream <- client.stream(sse ++ idHeader("session-1") ++ current)(_ => ())
        _ = stream.close()
        removed <- client.delete(idHeader("session-1") ++ current)
        _ <- opened(client, "session-2")
        bare <- client.delete(idHeader("session-2"))
      yield
        assertEquals(stream.status, 200)
        assertEquals(removed.status, 204)
        assertEquals(bare.status, 204)

  test("http: a notification is accepted with no body"):
    withServer: (client, _, _) =>
      for
        _ <- opened(client, "session-1")
        reply <- client.post(Fixtures.initializedNotification, idHeader("session-1"))
      yield
        assertEquals(reply.status, 202)
        assertEquals(reply.body, "")

  test("http: a request is answered with its reply as JSON"):
    withServer: (client, _, _) =>
      for
        _ <- opened(client, "session-1")
        _ <- client.post(Fixtures.initializedNotification, idHeader("session-1"))
        reply <- client.post(Fixtures.request(2, "tools/list"), idHeader("session-1"))
      yield
        assertEquals(reply.status, 200)
        assertEquals(reply.contentType, Some("application/json"))
        assertEquals(reply.body, s"""{"jsonrpc":"2.0","id":2,"result":{"tools":[${Fixtures.echoToolJson}]}}""")

  test("http: a verb we do not serve is not allowed"):
    withServer: (client, _, _) =>
      client.send("PUT", Some("{}"), Map.empty).map(reply => assertEquals(reply.status, 405))

  // --- GET and the event stream -----------------------------------------------------------------

  test("http: a GET that does not accept events is not allowed"):
    withServer: (client, _, _) =>
      for
        _ <- opened(client, "session-1")
        reply <- client.send("GET", None, idHeader("session-1"))
      yield
        assertEquals(reply.status, 405)
        assertEquals(reply.body, "")

  test("http: a GET with no session header is a bad request, and an unknown one is not found"):
    withServer: (client, _, _) =>
      for
        missing <- client.send("GET", None, sse)
        unknown <- client.send("GET", None, sse ++ idHeader("session-nope"))
      yield
        assertEquals(missing.status, 400)
        assertEquals(unknown.status, 404)

  test("http: a GET opens an event stream"):
    withServer: (client, _, _) =>
      for
        _ <- opened(client, "session-1")
        stream <- client.stream(sse ++ idHeader("session-1"))(_ => ())
      yield
        assertEquals(stream.status, 200)
        assertEquals(stream.headers.get("content-type"), Some("text/event-stream"))
        stream.close()

  test("http: a second concurrent GET for the same session is a conflict"):
    withServer: (client, _, _) =>
      for
        _ <- opened(client, "session-1")
        first <- client.stream(sse ++ idHeader("session-1"))(_ => ())
        second <- client.stream(sse ++ idHeader("session-1"))(_ => ())
      yield
        assertEquals(first.status, 200)
        assertEquals(second.status, 409)
        first.close()

  test("http: sink traffic queued before the stream opens is flushed when it does"):
    withServer: (client, _, sessions) =>
      val events = Events()
      for
        _ <- opened(client, "session-1")
        _ = sessions.first.ping(_ => ())
        _ = sessions.first.log(LoggingLevel.Info, None, Json.Str("queued"))
        stream <- client.stream(sse ++ idHeader("session-1"))(events.record)
        seen <- events.awaiting(2)
      yield
        assertEquals(stream.status, 200)
        assertEquals(seen(0), """{"jsonrpc":"2.0","id":1,"method":"ping"}""")
        assertEquals(
          seen(1),
          """{"jsonrpc":"2.0","method":"notifications/message","params":{"level":"info","data":"queued"}}"""
        )
        stream.close()

  test("http: sink traffic while the stream is open goes straight down it"):
    withServer: (client, _, sessions) =>
      val events = Events()
      for
        _ <- opened(client, "session-1")
        stream <- client.stream(sse ++ idHeader("session-1"))(events.record)
        _ = sessions.first.log(LoggingLevel.Error, Some("db"), Json.Str("live"))
        seen <- events.awaiting(1)
      yield
        assertEquals(
          seen,
          Vector(
            """{"jsonrpc":"2.0","method":"notifications/message","params":{"level":"error","logger":"db","data":"live"}}"""
          )
        )
        stream.close()

  test("http: a tool's mid-call notifications arrive on the GET stream, not on the POST"):
    // The documented consequence of never answering a POST with a stream: progress does not
    // precede the reply on the wire, it goes to the other channel.
    withServer: (client, _, _) =>
      val events = Events()
      val call = Fixtures.request(
        3,
        "tools/call",
        Some("""{"_meta":{"progressToken":"p-1"},"name":"echo","arguments":{"value":"x","notify":true}}""")
      )
      for
        _ <- opened(client, "session-1")
        _ <- client.post(Fixtures.initializedNotification, idHeader("session-1"))
        stream <- client.stream(sse ++ idHeader("session-1"))(events.record)
        reply <- client.post(call, idHeader("session-1"))
        seen <- events.awaiting(2)
      yield
        assertEquals(stream.status, 200)
        assertEquals(reply.status, 200)
        assertEquals(reply.body, """{"jsonrpc":"2.0","id":3,"result":{"content":[{"type":"text","text":"x"}]}}""")
        assert(seen(0).contains("notifications/progress"), seen(0))
        assert(seen(1).contains("notifications/message"), seen(1))
        stream.close()

  test("http: a server ping goes down the stream and its answer comes back on a POST"):
    withServer: (client, _, sessions) =>
      val events = Events()
      val answered = Promise[Boolean]()
      for
        _ <- opened(client, "session-1")
        stream <- client.stream(sse ++ idHeader("session-1"))(events.record)
        _ = sessions.first.ping(outcome => answered.success(outcome.isRight))
        asked <- events.awaiting(1)
        posted <- client.post("""{"jsonrpc":"2.0","id":1,"result":{}}""", idHeader("session-1"))
        ok <- answered.future
      yield
        assertEquals(asked, Vector("""{"jsonrpc":"2.0","id":1,"method":"ping"}"""))
        // A response earns no reply, so the POST that carried it is merely accepted.
        assertEquals(posted.status, 202)
        assertEquals(posted.body, "")
        assert(ok)
        assertEquals(sessions.first.pendingRequests, 0)
        stream.close()

  // --- DELETE and the whole lifecycle -----------------------------------------------------------

  test("http: DELETE terminates the session, and it is gone afterwards"):
    withServer: (client, server, _) =>
      for
        _ <- opened(client, "session-1")
        removed <- client.delete(idHeader("session-1"))
        after <- client.post(Fixtures.request(2, "tools/list"), idHeader("session-1"))
      yield
        assertEquals(removed.status, 204)
        assertEquals(removed.body, "")
        assertEquals(server.sessionCount, 0)
        assertEquals(after.status, 404)

  test("http: DELETE with no session header is a bad request, and an unknown one is not found"):
    withServer: (client, _, _) =>
      for
        missing <- client.delete()
        unknown <- client.delete(idHeader("session-nope"))
      yield
        assertEquals(missing.status, 400)
        assertEquals(unknown.status, 404)

  test("http: the whole lifecycle, end to end"):
    withServer: (client, server, _) =>
      for
        started <- client.post(Fixtures.initializeRequest)
        id = started.sessionId.getOrElse(fail("no session id on the initialize response"))
        ready <- client.post(Fixtures.initializedNotification, idHeader(id))
        listed <- client.post(Fixtures.request(2, "tools/list"), idHeader(id))
        called <- client.post(
          Fixtures.request(3, "tools/call", Some("""{"name":"echo","arguments":{"value":"round trip"}}""")),
          idHeader(id)
        )
        removed <- client.delete(idHeader(id))
        after <- client.post(Fixtures.request(4, "tools/list"), idHeader(id))
      yield
        assertEquals(started.status, 200)
        assertEquals(started.body, Fixtures.initializeResult)
        assertEquals(ready.status, 202)
        assertEquals(listed.status, 200)
        assertEquals(listed.body, s"""{"jsonrpc":"2.0","id":2,"result":{"tools":[${Fixtures.echoToolJson}]}}""")
        assertEquals(called.status, 200)
        assertEquals(
          called.body,
          """{"jsonrpc":"2.0","id":3,"result":{"content":[{"type":"text","text":"round trip"}]}}"""
        )
        assertEquals(removed.status, 204)
        assertEquals(after.status, 404)
        assertEquals(server.sessionCount, 0)

  test("http: two sessions do not see each other's traffic"):
    withServer: (client, _, sessions) =>
      val first = Events()
      val second = Events()
      for
        _ <- client.post(Fixtures.initializeRequest)
        _ <- client.post(Fixtures.initializeRequest)
        streamOne <- client.stream(sse ++ idHeader("session-1"))(first.record)
        streamTwo <- client.stream(sse ++ idHeader("session-2"))(second.record)
        _ = sessions.first.log(LoggingLevel.Info, None, Json.Str("for the first"))
        seen <- first.awaiting(1)
      yield
        assertEquals(streamOne.status, 200)
        assertEquals(streamTwo.status, 200)
        assert(seen.head.contains("for the first"), seen.head)
        assertEquals(second.all, Vector.empty[String])
        streamOne.close()
        streamTwo.close()

  test("http: a session's own state is its own"):
    withServer: (client, _, _) =>
      for
        _ <- client.post(Fixtures.initializeRequest)
        _ <- client.post(Fixtures.initializeRequest)
        _ <- client.post(Fixtures.initializedNotification, idHeader("session-1"))
        first <- client.post(Fixtures.request(2, "tools/list"), idHeader("session-1"))
        second <- client.post(Fixtures.request(2, "tools/list"), idHeader("session-2"))
      yield
        // The first finished its handshake; the second never did, and is told so.
        assertEquals(first.status, 200)
        assert(first.body.contains("\"result\""), first.body)
        assertEquals(second.status, 200)
        assert(second.body.contains("Server is not initialized"), second.body)

  test("http: malformed JSON is answered with a JSON-RPC parse error, not an HTTP failure"):
    // JSON-RPC-over-HTTP: the transport succeeded, the message did not.
    withServer: (client, _, _) =>
      for
        _ <- opened(client, "session-1")
        reply <- client.post("{not json", idHeader("session-1"))
      yield
        assertEquals(reply.status, 200)
        assertEquals(
          reply.body,
          """{"jsonrpc":"2.0","id":null,"error":{"code":-32700,"message":"Parse error","data":"$: not valid JSON"}}"""
        )
