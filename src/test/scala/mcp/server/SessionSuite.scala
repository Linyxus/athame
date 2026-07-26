package mcp.server

import mcp.*

/** The session core (V1, V3, V8).
  *
  * Everything here is pure: text in, text out, no clock and no I/O, so whole conversations are
  * pinned byte for byte. The error objects in particular are pinned in full — a client reads those
  * codes to decide what to do next, so they are as much a contract as the results are.
  */
class SessionSuite extends ServerSuite:

  // --- handshake -------------------------------------------------------------------------------

  test("handshake: initialize is answered on the return, never through the sink"):
    val harness = Harness()
    assertEquals(harness.session.state, SessionState.New)
    assertEquals(single(harness.receive(Fixtures.initializeRequest)), Fixtures.initializeResult)
    assertEquals(harness.sink, Vector.empty[String])
    assertEquals(harness.session.state, SessionState.Initializing)

  test("handshake: the initialized notification completes it and earns no reply"):
    val harness = Harness()
    harness.receive(Fixtures.initializeRequest)
    assertSilent(harness.receive(Fixtures.initializedNotification))
    assertEquals(harness.session.state, SessionState.Ready)
    assertEquals(harness.sink, Vector.empty[String])

  test("handshake: the client's identity is remembered"):
    val harness = Harness().ready()
    assertEquals(harness.session.clientInfo, Some(Fixtures.clientInfo))

  test("handshake: the declared capabilities are what the result carries"):
    val harness = Harness()
    harness.receive(Fixtures.initializeRequest)
    assertEquals(
      harness.session.capabilities,
      ServerCapabilities.empty.copy(logging = Some(Json.obj()), tools = Some(ToolsCapability(None)))
    )

  test("handshake: tools are declared exactly when a provider was supplied"):
    // The capability is derived, not trusted: the handshake cannot promise tools nobody serves.
    assertEquals(Harness(tools = None).session.capabilities.tools, None)
    assertEquals(Harness().session.capabilities.tools, Some(ToolsCapability(None)))
    val overclaiming = ServerCapabilities.empty.copy(tools = Some(ToolsCapability(Some(true))))
    assertEquals(Harness(tools = None, capabilities = overclaiming).session.capabilities.tools, None)

  test("handshake: a client on this revision gets it echoed back"):
    val harness = Harness()
    assertEquals(single(harness.receive(Fixtures.initializeRequest)), Fixtures.initializeResult)

  test("handshake: a client on an older revision is answered with ours, not refused"):
    // Version negotiation is the client's decision to make; the server states what it speaks and
    // the client decides whether it can live with it.
    val harness = Harness()
    val older =
      """{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2024-11-05","capabilities":{},"clientInfo":{"name":"test-client","version":"1.0.0"}}}"""
    assertEquals(single(harness.receive(older)), Fixtures.initializeResult)
    assertEquals(harness.session.state, SessionState.Initializing)

  test("handshake: instructions ride along when the server has any"):
    val harness = Harness(instructions = Some("Use echo."))
    assertEquals(
      single(harness.receive(Fixtures.initializeRequest)),
      """{"jsonrpc":"2.0","id":1,"result":{"protocolVersion":"2025-11-25","capabilities":{"logging":{},"tools":{}},"serverInfo":{"name":"athame-test","version":"0.1.0"},"instructions":"Use echo."}}"""
    )

  // --- state gates -----------------------------------------------------------------------------

  test("state: ping is answered in every state, handshake or no handshake"):
    val ping = Fixtures.request(9, "ping")
    val answer = """{"jsonrpc":"2.0","id":9,"result":{}}"""
    val fresh = Harness()
    assertEquals(single(fresh.receive(ping)), answer)
    val initializing = Harness()
    initializing.receive(Fixtures.initializeRequest)
    assertEquals(single(initializing.receive(ping)), answer)
    assertEquals(single(Harness().ready().receive(ping)), answer)

  test("state: every other request before the handshake is refused"):
    for premature <- Vector(
        Fixtures.request(2, "tools/list"),
        Fixtures.request(3, "tools/call", Some("""{"name":"echo"}""")),
        Fixtures.request(4, "logging/setLevel", Some("""{"level":"info"}"""))
      )
    do
      val harness = Harness()
      val reply = single(harness.receive(premature))
      assert(reply.contains("\"code\":-32600"), reply)
      assert(reply.contains("Server is not initialized"), reply)

  test("state: a request between initialize and initialized is still too early"):
    val harness = Harness()
    harness.receive(Fixtures.initializeRequest)
    assertEquals(
      single(harness.receive(Fixtures.request(2, "tools/list"))),
      """{"jsonrpc":"2.0","id":2,"error":{"code":-32600,"message":"Server is not initialized"}}"""
    )

  test("state: initializing twice is refused, in both later states"):
    val initializing = Harness()
    initializing.receive(Fixtures.initializeRequest)
    assertEquals(
      single(initializing.receive(Fixtures.initializeRequest)),
      """{"jsonrpc":"2.0","id":1,"error":{"code":-32600,"message":"Already initialized"}}"""
    )
    val ready = Harness().ready()
    assert(single(ready.receive(Fixtures.initializeRequest)).contains("Already initialized"))

  test("state: an initialized notification before initialize is dropped, not obeyed"):
    val harness = Harness()
    assertSilent(harness.receive(Fixtures.initializedNotification))
    assertEquals(harness.session.state, SessionState.New)

  test("state: a second initialized notification leaves a ready session ready"):
    val harness = Harness().ready()
    assertSilent(harness.receive(Fixtures.initializedNotification))
    assertEquals(harness.session.state, SessionState.Ready)

  test("state: cancellation is accepted and ignored, because nothing is ever in flight"):
    val harness = Harness().ready()
    assertSilent(harness.receive("""{"jsonrpc":"2.0","method":"notifications/cancelled","params":{"requestId":1}}"""))
    assertEquals(harness.sink, Vector.empty[String])
    assertEquals(harness.session.state, SessionState.Ready)

  test("state: a client progress notification is accepted and ignored"):
    val harness = Harness().ready()
    assertSilent(
      harness.receive("""{"jsonrpc":"2.0","method":"notifications/progress","params":{"progressToken":"p","progress":1}}""")
    )
    assertEquals(harness.sink, Vector.empty[String])

  // --- tools -----------------------------------------------------------------------------------

  test("tools/list: the provider's tools, with no pagination"):
    val harness = Harness().ready()
    assertEquals(
      single(harness.receive(Fixtures.request(2, "tools/list"))),
      s"""{"jsonrpc":"2.0","id":2,"result":{"tools":[${Fixtures.echoToolJson}]}}"""
    )

  test("tools/list: a cursor is ignored rather than refused"):
    // One page is the whole list; a client that pages anyway gets the same answer and no cursor
    // back, which tells it there is no more.
    val harness = Harness().ready()
    val paged = Fixtures.request(2, "tools/list", Some("""{"cursor":"anything"}"""))
    assertEquals(
      single(harness.receive(paged)),
      s"""{"jsonrpc":"2.0","id":2,"result":{"tools":[${Fixtures.echoToolJson}]}}"""
    )

  test("tools/call: arguments go through and the result comes back"):
    val harness = Harness().ready()
    val call = Fixtures.request(3, "tools/call", Some("""{"name":"echo","arguments":{"value":"hi"}}"""))
    assertEquals(
      single(harness.receive(call)),
      """{"jsonrpc":"2.0","id":3,"result":{"content":[{"type":"text","text":"hi"}]}}"""
    )

  test("tools/call: a tool that fails returns a result, not a protocol error"):
    // The model has to be able to see this and correct itself, which it cannot do with an error
    // response.
    val harness = Harness().ready()
    val call =
      Fixtures.request(3, "tools/call", Some("""{"name":"echo","arguments":{"value":"hi","fail":true}}"""))
    assertEquals(
      single(harness.receive(call)),
      """{"jsonrpc":"2.0","id":3,"result":{"content":[{"type":"text","text":"failed: hi"}],"isError":true}}"""
    )

  test("tools/call: a tool nobody offers is a protocol error naming it"):
    val harness = Harness().ready()
    val call = Fixtures.request(3, "tools/call", Some("""{"name":"nope"}"""))
    assertEquals(
      single(harness.receive(call)),
      """{"jsonrpc":"2.0","id":3,"error":{"code":-32602,"message":"Unknown tool: nope"}}"""
    )

  test("tools/call: a provider that gives up is an internal error, and the model never sees it"):
    val harness = Harness().ready()
    val call = Fixtures.request(3, "tools/call", Some("""{"name":"echo","arguments":{"break":true}}"""))
    assertEquals(
      single(harness.receive(call)),
      """{"jsonrpc":"2.0","id":3,"error":{"code":-32603,"message":"provider exploded"}}"""
    )

  test("tools/call: a provider that throws is caught, and the stack stays on this side"):
    val harness = Harness().ready()
    val call = Fixtures.request(3, "tools/call", Some("""{"name":"echo","arguments":{"throw":true}}"""))
    val reply = single(harness.receive(call))
    assertEquals(reply, """{"jsonrpc":"2.0","id":3,"error":{"code":-32603,"message":"kaboom"}}""")
    assert(!reply.contains("at "), reply)
    // The session is still usable afterwards; a thrown handler is not a poisoned session.
    assertEquals(harness.session.state, SessionState.Ready)
    assert(single(harness.receive(Fixtures.request(4, "tools/list"))).contains("\"result\""))

  test("tools/call: mid-call notifications go through the sink, ahead of the reply"):
    val harness = Harness().ready()
    val call = Fixtures.request(
      4,
      "tools/call",
      Some("""{"_meta":{"progressToken":"p-1"},"name":"echo","arguments":{"value":"x","notify":true}}""")
    )
    val reply = single(harness.receive(call))
    assertEquals(
      harness.sink,
      Vector(
        """{"jsonrpc":"2.0","method":"notifications/progress","params":{"progressToken":"p-1","progress":1,"total":2,"message":"halfway"}}""",
        """{"jsonrpc":"2.0","method":"notifications/message","params":{"level":"info","logger":"echo","data":"working"}}"""
      )
    )
    assertEquals(reply, """{"jsonrpc":"2.0","id":4,"result":{"content":[{"type":"text","text":"x"}]}}""")

  test("tools/call: progress is silent when the client never asked for it"):
    // A progress notification has nowhere to point without a token, so there is no message to
    // send rather than a message to suppress. The log still goes.
    val harness = Harness().ready()
    val call =
      Fixtures.request(4, "tools/call", Some("""{"name":"echo","arguments":{"value":"x","notify":true}}"""))
    harness.receive(call)
    assertEquals(
      harness.sink,
      Vector("""{"jsonrpc":"2.0","method":"notifications/message","params":{"level":"info","logger":"echo","data":"working"}}""")
    )

  test("tools: a server with no provider says the capability was never declared"):
    val harness = Harness(tools = None, capabilities = ServerCapabilities.empty).ready()
    for call <- Vector(
        Fixtures.request(2, "tools/list"),
        Fixtures.request(3, "tools/call", Some("""{"name":"echo"}"""))
      )
    do
      val reply = single(harness.receive(call))
      assert(reply.contains("\"code\":-32600"), reply)
      assert(reply.contains("Tools capability not declared"), reply)

  // --- logging ---------------------------------------------------------------------------------

  test("logging: with no level set, everything is emitted"):
    val harness = Harness().ready()
    assertEquals(harness.session.loggingLevel, None)
    for level <- LoggingLevel.values do harness.session.log(level, None, Json.Str("x"))
    assertEquals(harness.sink.length, LoggingLevel.values.length)

  test("logging: setLevel is answered with an empty result and remembered"):
    val harness = Harness().ready()
    assertEquals(
      single(harness.receive(Fixtures.request(2, "logging/setLevel", Some("""{"level":"warning"}""")))),
      """{"jsonrpc":"2.0","id":2,"result":{}}"""
    )
    assertEquals(harness.session.loggingLevel, Some(LoggingLevel.Warning))

  test("logging: every level gates exactly the levels at or above it"):
    for minimum <- LoggingLevel.values do
      val harness = Harness().ready()
      harness.receive(Fixtures.request(2, "logging/setLevel", Some(s"""{"level":"${minimum.wire}"}""")))
      harness.clearSink()
      for level <- LoggingLevel.values do harness.session.log(level, None, Json.Str(level.wire))
      val emitted = LoggingLevel.values.toVector.filter(_.severity >= minimum.severity)
      assertEquals(harness.sink.length, emitted.length, s"at minimum ${minimum.wire}")
      for level <- emitted do assert(harness.sink.exists(_.contains(s""""${level.wire}"""")), level.wire)

  test("logging: the emitted notification is a whole notifications/message"):
    val harness = Harness().ready()
    harness.session.log(LoggingLevel.Error, Some("db"), Json.obj("msg" -> Json.Str("disk full")))
    assertEquals(
      harness.sink,
      Vector(
        """{"jsonrpc":"2.0","method":"notifications/message","params":{"level":"error","logger":"db","data":{"msg":"disk full"}}}"""
      )
    )

  test("logging: setLevel is refused when the capability was never declared"):
    // The method exists and we understood it perfectly well; we just never offered it. That is a
    // bad request, not a missing method.
    val harness = Harness(capabilities = ServerCapabilities.empty).ready()
    assertEquals(
      single(harness.receive(Fixtures.request(2, "logging/setLevel", Some("""{"level":"info"}""")))),
      """{"jsonrpc":"2.0","id":2,"error":{"code":-32600,"message":"Logging capability not declared"}}"""
    )
    assertEquals(harness.session.loggingLevel, None)

  // --- error mapping ---------------------------------------------------------------------------

  test("errors: malformed text is a parse error with a null id"):
    // JSON-RPC reserves the null id for exactly this: nothing parsed, so there is no id to echo.
    val harness = Harness().ready()
    assertEquals(
      single(harness.receive("{not json")),
      """{"jsonrpc":"2.0","id":null,"error":{"code":-32700,"message":"Parse error","data":"$: not valid JSON"}}"""
    )

  test("errors: an unknown method on a request is method-not-found, naming the method"):
    val harness = Harness().ready()
    assertEquals(
      single(harness.receive(Fixtures.request(7, "tools/frobnicate"))),
      """{"jsonrpc":"2.0","id":7,"error":{"code":-32601,"message":"Method not found: tools/frobnicate","data":"$.method: unknown method \"tools/frobnicate\""}}"""
    )

  test("errors: a real MCP method this server does not implement is also method-not-found"):
    val harness = Harness().ready()
    val reply = single(harness.receive(Fixtures.request(7, "resources/list")))
    assert(reply.contains("\"code\":-32601"), reply)
    assert(reply.contains("Method not found: resources/list"), reply)

  test("errors: an unknown method on a notification earns no reply at all"):
    // JSON-RPC forbids answering a notification, however wrong it is.
    val harness = Harness().ready()
    assertSilent(harness.receive("""{"jsonrpc":"2.0","method":"notifications/unheard-of"}"""))
    assertEquals(harness.sink, Vector.empty[String])

  test("errors: a request whose params will not decode is invalid params, against its own id"):
    val harness = Harness().ready()
    assertEquals(
      single(harness.receive(Fixtures.request(8, "tools/call", Some("""{"arguments":{}}""")))),
      """{"jsonrpc":"2.0","id":8,"error":{"code":-32602,"message":"Invalid params","data":"$.params.name: missing required field"}}"""
    )

  test("errors: a request with no params at all where they are required is the same thing"):
    val harness = Harness().ready()
    assertEquals(
      single(harness.receive(Fixtures.request(8, "tools/call"))),
      """{"jsonrpc":"2.0","id":8,"error":{"code":-32602,"message":"Invalid params","data":"$.params: missing required field"}}"""
    )

  test("errors: an id we cannot echo makes it an invalid request, with a null id"):
    val harness = Harness().ready()
    assertEquals(
      single(harness.receive("""{"jsonrpc":"2.0","id":[],"method":"ping"}""")),
      """{"jsonrpc":"2.0","id":null,"error":{"code":-32600,"message":"Invalid Request","data":"$.id: expected a string or number, found array"}}"""
    )

  test("errors: something that is not a JSON-RPC message at all is an invalid request"):
    val harness = Harness().ready()
    val reply = single(harness.receive("""{"hello":"world"}"""))
    assert(reply.startsWith("""{"jsonrpc":"2.0","id":null,"error":{"code":-32600"""), reply)

  test("errors: a method that is not a string is an invalid request, not a notification"):
    // Ruling V1.1, and JSON-RPC 2.0's own example of an invalid request. Classifying a message as
    // a notification on the mere *presence* of a `method` member would drop this silently, which
    // is the one outcome the spec rules out: it is not a notification, so it is owed an answer,
    // and having no id to echo that answer carries the null.
    val harness = Harness().ready()
    assertEquals(
      single(harness.receive("""{"jsonrpc":"2.0","method":1}""")),
      """{"jsonrpc":"2.0","id":null,"error":{"code":-32600,"message":"Invalid Request","data":"$.method: expected a string, found number"}}"""
    )

  test("errors: the three V1.1 rows, side by side"):
    val harness = Harness().ready()
    // A notification whose params will not decode: silence, whatever is wrong with it.
    assertSilent(harness.receive("""{"jsonrpc":"2.0","method":"notifications/progress","params":{}}"""))
    // Not classifiable as a notification: answered, with the null id.
    assert(single(harness.receive("""{"jsonrpc":"2.0","method":1}""")).contains("\"code\":-32600"))
    // Nothing parsed at all: answered, with the null id.
    assert(single(harness.receive("{")).contains("\"code\":-32700"))

  test("errors: a notification with undecodable params is dropped, not answered"):
    // The id-recovery rule decides who a refusal is addressed to, and a notification has nobody.
    val harness = Harness().ready()
    assertSilent(harness.receive("""{"jsonrpc":"2.0","method":"notifications/progress","params":{}}"""))

  test("errors: a wrong jsonrpc version is an invalid request"):
    val harness = Harness().ready()
    val reply = single(harness.receive("""{"jsonrpc":"1.0","id":1,"method":"ping"}"""))
    assertEquals(
      reply,
      """{"jsonrpc":"2.0","id":1,"error":{"code":-32602,"message":"Invalid params","data":"$.jsonrpc: expected \"2.0\", found \"1.0\""}}"""
    )

  test("errors: a top-level array is refused, batching being gone from MCP"):
    val harness = Harness().ready()
    val reply = single(harness.receive("[]"))
    assert(reply.contains("\"id\":null"), reply)
    assert(reply.contains("single JSON-RPC message"), reply)

  test("errors: every refusal is still a well-formed message the client can decode"):
    val harness = Harness().ready()
    val refusals = Vector(
      "{not json",
      "[]",
      """{"hello":"world"}""",
      """{"jsonrpc":"2.0","id":[],"method":"ping"}""",
      Fixtures.request(1, "nope"),
      Fixtures.request(2, "tools/call")
    )
    for text <- refusals do
      val reply = single(harness.receive(text))
      assert(Codec.decodeServerMessage(reply).isRight, s"$text produced undecodable $reply")
