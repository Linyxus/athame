package mcp

/** The envelope and method dispatch (schema.ts:124-170, 2494-2573).
  *
  * This is the layer a transport hands bytes to, so its refusals matter as much as its successes:
  * an unknown method has to be distinguishable from a malformed one, because the first deserves
  * `-32601` and the second deserves `-32700`.
  */
class MessagesSuite extends McpSuite:

  private def client(text: String)(using munit.Location): ClientMessage =
    Codec.decodeClientMessage(text) match
      case Right(message) => message
      case Left(error)    => fail(s"expected $text to decode, got ${error.render}")

  private def clientFails(text: String)(using munit.Location): String =
    Codec.decodeClientMessage(text) match
      case Left(error)    => error.render
      case Right(message) => fail(s"expected $text to be refused, got $message")

  private def serverFails(text: String)(using munit.Location): String =
    Codec.decodeServerMessage(text) match
      case Left(error)    => error.render
      case Right(message) => fail(s"expected $text to be refused, got $message")

  private def assertClientGolden(text: String, message: ClientMessage)(using munit.Location): Unit =
    assertEquals(Codec.decodeClientMessage(text), Right(message))
    assertEquals(Codec.encodeClientMessage(message), text)

  private def assertServerGolden(text: String, message: ServerMessage)(using munit.Location): Unit =
    assertEquals(Codec.decodeServerMessage(text), Right(message))
    assertEquals(Codec.encodeServerMessage(message), text)

  // --- one pinned envelope per method ---------------------------------------------------------

  test("methods: every request a client can send, as a whole message"):
    assertClientGolden(
      """{"jsonrpc":"2.0","id":1,"method":"ping"}""",
      ClientMessage.Request(RequestId.Num(1), ClientRequest.Ping(None))
    )
    assertClientGolden(
      s"""{"jsonrpc":"2.0","id":1,"method":"initialize","params":${Golden.initializeMinimal}}""",
      ClientMessage.Request(RequestId.Num(1), ClientRequest.Initialize(Sample.initializeMinimal))
    )
    assertClientGolden(
      s"""{"jsonrpc":"2.0","id":1,"method":"logging/setLevel","params":${Golden.setLevel}}""",
      ClientMessage.Request(
        RequestId.Num(1),
        ClientRequest.SetLevel(SetLevelRequestParams(None, LoggingLevel.Warning))
      )
    )
    assertClientGolden(
      s"""{"jsonrpc":"2.0","id":1,"method":"tools/call","params":${Golden.callToolMinimal}}""",
      ClientMessage.Request(RequestId.Num(1), ClientRequest.CallTool(Sample.callToolMinimal))
    )
    assertClientGolden(
      """{"jsonrpc":"2.0","id":1,"method":"tools/list"}""",
      ClientMessage.Request(RequestId.Num(1), ClientRequest.ListTools(None))
    )
    assertClientGolden(
      """{"jsonrpc":"2.0","id":1,"method":"tools/list","params":{"cursor":"c-1"}}""",
      ClientMessage.Request(
        RequestId.Num(1),
        ClientRequest.ListTools(Some(PaginatedRequestParams(None, Some("c-1"))))
      )
    )

  test("methods: every notification a client can send, as a whole message"):
    assertClientGolden(
      """{"jsonrpc":"2.0","method":"notifications/cancelled","params":{}}""",
      ClientMessage.Notification(ClientNotification.Cancelled(Sample.cancelledMinimal))
    )
    assertClientGolden(
      s"""{"jsonrpc":"2.0","method":"notifications/progress","params":${Golden.progressMinimal}}""",
      ClientMessage.Notification(ClientNotification.Progress(Sample.progressMinimal))
    )
    assertClientGolden(
      """{"jsonrpc":"2.0","method":"notifications/initialized"}""",
      ClientMessage.Notification(ClientNotification.Initialized(None))
    )

  test("methods: every request a server can send, as a whole message"):
    assertServerGolden(
      """{"jsonrpc":"2.0","id":1,"method":"ping"}""",
      ServerMessage.Request(RequestId.Num(1), ServerRequest.Ping(None))
    )
    assertServerGolden(
      s"""{"jsonrpc":"2.0","id":1,"method":"sampling/createMessage","params":${Golden.createMessageMinimal}}""",
      ServerMessage.Request(RequestId.Num(1), ServerRequest.CreateMessage(Sample.createMessageMinimal))
    )
    assertServerGolden(
      s"""{"jsonrpc":"2.0","id":1,"method":"elicitation/create","params":${Golden.elicitForm}}""",
      ServerMessage.Request(RequestId.Num(1), ServerRequest.Elicit(Sample.elicitForm))
    )

  test("methods: every notification a server can send, as a whole message"):
    assertServerGolden(
      s"""{"jsonrpc":"2.0","method":"notifications/cancelled","params":${Golden.cancelledFull}}""",
      ServerMessage.Notification(ServerNotification.Cancelled(Sample.cancelledFull))
    )
    assertServerGolden(
      s"""{"jsonrpc":"2.0","method":"notifications/progress","params":${Golden.progressFull}}""",
      ServerMessage.Notification(ServerNotification.Progress(Sample.progressFull))
    )
    assertServerGolden(
      s"""{"jsonrpc":"2.0","method":"notifications/message","params":${Golden.loggingMinimal}}""",
      ServerMessage.Notification(ServerNotification.LoggingMessage(Sample.loggingMinimal))
    )
    assertServerGolden(
      """{"jsonrpc":"2.0","method":"notifications/tools/list_changed"}""",
      ServerMessage.Notification(ServerNotification.ToolListChanged(None))
    )
    assertServerGolden(
      s"""{"jsonrpc":"2.0","method":"notifications/elicitation/complete","params":${Golden.elicitationComplete}}""",
      ServerMessage.Notification(
        ServerNotification.ElicitationComplete(ElicitationCompleteNotificationParams("e-1"))
      )
    )

  test("methods: every result, packed into a response and pinned"):
    assertServerGolden(
      """{"jsonrpc":"2.0","id":1,"result":{}}""",
      ServerMessage.response(RequestId.Num(1), ServerResult.Empty(EmptyResult.empty))
    )
    assertServerGolden(
      s"""{"jsonrpc":"2.0","id":1,"result":${Golden.initializeResultMinimal}}""",
      ServerMessage.response(RequestId.Num(1), ServerResult.Initialize(Sample.initializeResultMinimal))
    )
    assertServerGolden(
      s"""{"jsonrpc":"2.0","id":1,"result":${Golden.callToolResultMinimal}}""",
      ServerMessage.response(RequestId.Num(1), ServerResult.CallTool(Sample.callToolResultMinimal))
    )
    assertServerGolden(
      s"""{"jsonrpc":"2.0","id":1,"result":${Golden.listToolsMinimal}}""",
      ServerMessage.response(RequestId.Num(1), ServerResult.ListTools(Sample.listToolsMinimal))
    )
    assertClientGolden(
      s"""{"jsonrpc":"2.0","id":1,"result":${Golden.createMessageResultMinimal}}""",
      ClientMessage.response(RequestId.Num(1), ClientResult.CreateMessage(Sample.createMessageResultMinimal))
    )
    assertClientGolden(
      s"""{"jsonrpc":"2.0","id":1,"result":${Golden.elicitResultMinimal}}""",
      ClientMessage.response(RequestId.Num(1), ClientResult.Elicit(Sample.elicitResultMinimal))
    )

  test("methods: the pinned envelopes above cover every method the package knows"):
    // If a method is added to Method without an envelope vector, this is the test that says so.
    val covered = Set(
      Method.Ping,
      Method.Initialize,
      Method.SetLevel,
      Method.CallTool,
      Method.ListTools,
      Method.CreateMessage,
      Method.Elicit,
      Method.Cancelled,
      Method.Progress,
      Method.Initialized,
      Method.LoggingMessage,
      Method.ToolListChanged,
      Method.ElicitationComplete
    )
    assertEquals(covered.size, 13)
    val reachable =
      (Sample.clientRequests.map(_.method) ++ Sample.serverRequests.map(_.method) ++
        Sample.clientNotifications.map(_.method) ++ Sample.serverNotifications.map(_.method)).toSet
    assertEquals(reachable, covered)

  // --- requests ------------------------------------------------------------------------------

  test("client request: initialize, in both directions"):
    val text = s"""{"jsonrpc":"2.0","id":1,"method":"initialize","params":${Golden.initializeMinimal}}"""
    val message = ClientMessage.Request(RequestId.Num(1), ClientRequest.Initialize(Sample.initializeMinimal))
    assertEquals(client(text), message)
    assertEquals(Codec.encodeClientMessage(message), text)

  test("client request: an id of 1 is written as 1, not 1.0"):
    val ping = ClientMessage.Request(RequestId.Num(1), ClientRequest.Ping(None))
    assert(Codec.encodeClientMessage(ping).contains("\"id\":1,"), Codec.encodeClientMessage(ping))

  test("client request: a request with no params omits the member entirely"):
    val text = """{"jsonrpc":"2.0","id":"a","method":"ping"}"""
    val message = ClientMessage.Request(RequestId.Str("a"), ClientRequest.Ping(None))
    assertEquals(client(text), message)
    assertEquals(Codec.encodeClientMessage(message), text)

  test("client request: params present but empty is not the same as params absent"):
    val text = """{"jsonrpc":"2.0","id":1,"method":"ping","params":{}}"""
    val message = ClientMessage.Request(RequestId.Num(1), ClientRequest.Ping(Some(RequestParams.empty)))
    assertEquals(client(text), message)
    assertEquals(Codec.encodeClientMessage(message), text)
    assertNotEquals(client(text), client("""{"jsonrpc":"2.0","id":1,"method":"ping"}"""))

  test("client request: every method this side answers to"):
    assertEquals(
      Sample.clientRequests.map(_.method).distinct,
      Vector("ping", "initialize", "logging/setLevel", "tools/call", "tools/list")
    )

  test("client request: tools/call, with the params under the right key"):
    val text = s"""{"jsonrpc":"2.0","id":2,"method":"tools/call","params":${Golden.callToolFull}}"""
    val message = ClientMessage.Request(RequestId.Num(2), ClientRequest.CallTool(Sample.callToolFull))
    assertEquals(client(text), message)
    assertEquals(Codec.encodeClientMessage(message), text)

  test("server request: sampling and elicitation, in both directions"):
    val sampling = s"""{"jsonrpc":"2.0","id":1,"method":"sampling/createMessage","params":${Golden.createMessageMinimal}}"""
    assertEquals(
      Codec.decodeServerMessage(sampling),
      Right(ServerMessage.Request(RequestId.Num(1), ServerRequest.CreateMessage(Sample.createMessageMinimal)))
    )
    val elicit = s"""{"jsonrpc":"2.0","id":"e","method":"elicitation/create","params":${Golden.elicitUrl}}"""
    assertEquals(
      Codec.decodeServerMessage(elicit),
      Right(ServerMessage.Request(RequestId.Str("e"), ServerRequest.Elicit(Sample.elicitUrl)))
    )
    assertEquals(
      Codec.encodeServerMessage(
        ServerMessage.Request(RequestId.Str("e"), ServerRequest.Elicit(Sample.elicitUrl))
      ),
      elicit
    )

  test("requests: a request whose params are required says so when they are missing"):
    assertEquals(
      clientFails("""{"jsonrpc":"2.0","id":1,"method":"initialize"}"""),
      "$.params: missing required field"
    )

  test("requests: a complaint inside params carries the whole path"):
    // The rendering contract from the design note, reached through a real message.
    val text =
      s"""{"jsonrpc":"2.0","id":1,"method":"sampling/createMessage","params":{"messages":[],"maxTokens":1,"tools":[${Golden.toolMinimal},${Golden.toolMinimal},{"name":5,"inputSchema":{"type":"object"}}]}}"""
    assertEquals(serverFails(text), "$.params.tools[2].name: expected a string, found number")

  // --- notifications -------------------------------------------------------------------------

  test("notification: no id, and params omitted when there are none"):
    val text = """{"jsonrpc":"2.0","method":"notifications/initialized"}"""
    val message = ClientMessage.Notification(ClientNotification.Initialized(None))
    assertEquals(client(text), message)
    assertEquals(Codec.encodeClientMessage(message), text)

  test("notification: a request without an id is read as a notification"):
    assertEquals(
      client("""{"jsonrpc":"2.0","method":"notifications/cancelled","params":{}}"""),
      ClientMessage.Notification(ClientNotification.Cancelled(Sample.cancelledMinimal))
    )

  test("notification: an id spelled null is an absent id, so the message is a notification"):
    assertEquals(
      client("""{"jsonrpc":"2.0","id":null,"method":"notifications/initialized"}"""),
      ClientMessage.Notification(ClientNotification.Initialized(None))
    )

  test("notification: every method each side answers to"):
    assertEquals(
      Sample.clientNotifications.map(_.method).distinct,
      Vector("notifications/cancelled", "notifications/progress", "notifications/initialized")
    )
    assertEquals(
      Sample.serverNotifications.map(_.method).distinct,
      Vector(
        "notifications/cancelled",
        "notifications/progress",
        "notifications/message",
        "notifications/tools/list_changed",
        "notifications/elicitation/complete"
      )
    )

  test("notification: a server log message, in both directions"):
    val text = s"""{"jsonrpc":"2.0","method":"notifications/message","params":${Golden.loggingFull}}"""
    val message = ServerMessage.Notification(ServerNotification.LoggingMessage(Sample.loggingFull))
    assertEquals(Codec.decodeServerMessage(text), Right(message))
    assertEquals(Codec.encodeServerMessage(message), text)

  // --- responses and errors ------------------------------------------------------------------

  test("response: the result stays JSON at this layer"):
    val text = s"""{"jsonrpc":"2.0","id":1,"result":${Golden.initializeResultMinimal}}"""
    val message =
      ServerMessage.Response(RequestId.Num(1), decoded(Golden.initializeResultMinimal, Decode.jsonObj))
    assertEquals(Codec.decodeServerMessage(text), Right(message))
    assertEquals(Codec.encodeServerMessage(message), text)

  test("response: a typed result packs into one, and its own decoder reads it back"):
    // The handoff a session layer will make: encode a typed result, and decode it again knowing
    // which request it answered.
    val message =
      ServerMessage.response(RequestId.Num(1), ServerResult.Initialize(Sample.initializeResultFull))
    val text = Codec.encodeServerMessage(message)
    assertEquals(text, s"""{"jsonrpc":"2.0","id":1,"result":${Golden.initializeResultFull}}""")
    message match
      case ServerMessage.Response(_, result) =>
        assertEquals(
          Codec.decode(Json.stringify(result), InitializeResult.decoder),
          Right(Sample.initializeResultFull)
        )
      case other => fail(s"expected a response, got $other")

  test("response: an empty result is an empty object, not an absent member"):
    assertEquals(
      Codec.encodeServerMessage(
        ServerMessage.response(RequestId.Num(1), ServerResult.Empty(EmptyResult.empty))
      ),
      """{"jsonrpc":"2.0","id":1,"result":{}}"""
    )

  test("response: a result that is not an object is refused"):
    // Ruling M11: Response carries Json.Obj rather than Json, because MCP's Result is an interface
    // and so always an object on the wire. This is the rendering that refinement buys.
    assertEquals(
      serverFails("""{"jsonrpc":"2.0","id":1,"result":5}"""),
      "$.result: expected an object, found number"
    )

  test("response: a response with no id is refused, because nothing could match it"):
    assertEquals(serverFails(s"""{"jsonrpc":"2.0","result":{}}"""), "$.id: missing required field")

  test("error response: id, code and message, in both directions"):
    val text = """{"jsonrpc":"2.0","id":1,"error":{"code":-32601,"message":"Method not found"}}"""
    val message = ServerMessage.Error(Some(RequestId.Num(1)), Sample.errorMinimal)
    assertEquals(Codec.decodeServerMessage(text), Right(message))
    assertEquals(Codec.encodeServerMessage(message), text)

  test("error response: the id is optional here, as the schema has it"):
    // Unlike a result response: a peer that could not parse the request has no id to echo.
    val text = """{"jsonrpc":"2.0","error":{"code":-32700,"message":"Parse error"}}"""
    val message =
      ServerMessage.Error(None, JsonRpc.Error(ErrorCode.ParseError, "Parse error", None))
    assertEquals(Codec.decodeServerMessage(text), Right(message))
    assertEquals(Codec.encodeServerMessage(message), text)

  test("error response: a broken error object is located inside it"):
    assertEquals(
      serverFails("""{"jsonrpc":"2.0","id":1,"error":{"message":"x"}}"""),
      "$.error.code: missing required field"
    )

  // --- dispatch failures ---------------------------------------------------------------------

  test("dispatch: an unknown method is its own error, so a dispatcher can answer -32601"):
    assertEquals(
      clientFails("""{"jsonrpc":"2.0","id":1,"method":"tools/frobnicate"}"""),
      """$.method: unknown method "tools/frobnicate""""
    )
    assertEquals(
      Codec.decodeClientMessage("""{"jsonrpc":"2.0","id":1,"method":"tools/frobnicate"}"""),
      Left(DecodeError.UnknownMethod(Path.root / "method", "tools/frobnicate"))
    )

  test("dispatch: a method MCP defines but this package skips is also unknown"):
    // Honest about scope: `resources/list` is a real method, and we do not model it.
    val skippedMethods =
      Vector("resources/list", "prompts/get", "completion/complete", "tasks/get", "roots/list")
    for skipped <- skippedMethods do
      assertEquals(
        Codec.decodeClientMessage(s"""{"jsonrpc":"2.0","id":1,"method":"$skipped"}"""),
        Left(DecodeError.UnknownMethod(Path.root / "method", skipped))
      )

  test("dispatch: a method belonging to the other direction is unknown on this one"):
    assertEquals(
      clientFails("""{"jsonrpc":"2.0","id":1,"method":"sampling/createMessage","params":{}}"""),
      """$.method: unknown method "sampling/createMessage""""
    )
    assertEquals(
      serverFails("""{"jsonrpc":"2.0","id":1,"method":"initialize","params":{}}"""),
      """$.method: unknown method "initialize""""
    )

  test("dispatch: ping goes both ways, being the one request either side may send"):
    assertEquals(
      Codec.decodeClientMessage("""{"jsonrpc":"2.0","id":1,"method":"ping"}"""),
      Right(ClientMessage.Request(RequestId.Num(1), ClientRequest.Ping(None)))
    )
    assertEquals(
      Codec.decodeServerMessage("""{"jsonrpc":"2.0","id":1,"method":"ping"}"""),
      Right(ServerMessage.Request(RequestId.Num(1), ServerRequest.Ping(None)))
    )

  test("envelope: the jsonrpc member is checked, not assumed"):
    assertEquals(
      clientFails("""{"jsonrpc":"1.0","id":1,"method":"ping"}"""),
      """$.jsonrpc: expected "2.0", found "1.0""""
    )
    assertEquals(clientFails("""{"id":1,"method":"ping"}"""), "$.jsonrpc: missing required field")

  test("envelope: a message that is none of the four shapes says which members it wanted"):
    assertEquals(
      clientFails("""{"jsonrpc":"2.0","id":1}"""),
      """$: expected a "method", "result" or "error" member, found object"""
    )

  test("envelope: a top-level array is refused, batching having been removed from MCP"):
    assertEquals(clientFails("[]"), "$: expected a single JSON-RPC message, found array")
    assertEquals(
      clientFails("""[{"jsonrpc":"2.0","id":1,"method":"ping"}]"""),
      "$: expected a single JSON-RPC message, found array"
    )
    assertEquals(serverFails("[]"), "$: expected a single JSON-RPC message, found array")

  test("envelope: anything that is not an object at all is refused too"):
    assertEquals(clientFails("null"), "$: expected an object, found null")
    assertEquals(clientFails("5"), "$: expected an object, found number")
    assertEquals(clientFails("\"ping\""), "$: expected an object, found string")

  test("envelope: malformed text is a parse failure and nothing more specific"):
    assertEquals(clientFails("{"), "$: not valid JSON")
    assertEquals(clientFails(""), "$: not valid JSON")
    assertEquals(clientFails("""{"jsonrpc":"2.0",}"""), "$: not valid JSON")

  test("envelope: unknown members are ignored, so a peer may extend the envelope"):
    assertEquals(
      client("""{"jsonrpc":"2.0","id":1,"method":"ping","extra":{"a":1}}"""),
      ClientMessage.Request(RequestId.Num(1), ClientRequest.Ping(None))
    )

  test("envelope: an id that is neither string nor number is refused"):
    assertEquals(
      clientFails("""{"jsonrpc":"2.0","id":[1],"method":"ping"}"""),
      "$.id: expected a string or number, found array"
    )

  // --- the result unions ---------------------------------------------------------------------

  test("result unions: every member encodes to what its own encoder produces"):
    assertEquals(
      Codec.encode(ServerResult.ListTools(Sample.listToolsFull), ServerResult.encoder),
      Golden.listToolsFull
    )
    assertEquals(
      Codec.encode(ClientResult.Elicit(Sample.elicitResultFull), ClientResult.encoder),
      Golden.elicitResultFull
    )
    assertEquals(Codec.encode(ClientResult.Empty(EmptyResult.empty), ClientResult.encoder), "{}")

  test("result unions: a packed response and a hand-built one are the same message"):
    assertEquals(
      ClientMessage.response(RequestId.Num(2), ClientResult.CreateMessage(Sample.createMessageResultMinimal)),
      ClientMessage.Response(RequestId.Num(2), decoded(Golden.createMessageResultMinimal, Decode.jsonObj))
    )
