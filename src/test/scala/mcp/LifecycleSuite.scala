package mcp

/** `initialize` and the capabilities it negotiates (schema.ts:249-568).
  *
  * Capabilities are the one part of MCP where absence and emptiness are different claims: a server
  * with `"tools":{}` offers tools, a server with no `tools` key does not. Every test here that pins
  * a `{}` is pinning that distinction.
  */
class LifecycleSuite extends McpSuite:

  // --- the shared records --------------------------------------------------------------------

  test("icon: src alone, and src with everything"):
    assertGolden(Golden.iconBare, Sample.iconBare, Icon.decoder, Icon.encoder)
    assertGolden(Golden.iconLight, Sample.iconLight, Icon.decoder, Icon.encoder)
    assertGolden(Golden.iconDark, Sample.iconDark, Icon.decoder, Icon.encoder)

  test("icon: src is required and the theme is a closed union"):
    assertEquals(rendered("{}", Icon.decoder), "$.src: missing required field")
    assertEquals(
      rendered("""{"src":"x","theme":"sepia"}""", Icon.decoder),
      """$.theme: expected one of "light", "dark", found "sepia""""
    )

  test("role: the two spellings, and nothing else"):
    assertEquals(decoded("\"user\"", Role.decoder), Role.User)
    assertEquals(decoded("\"assistant\"", Role.decoder), Role.Assistant)
    assertEquals(Codec.encode(Role.User, Role.encoder), "\"user\"")
    assertEquals(Codec.encode(Role.Assistant, Role.encoder), "\"assistant\"")
    assertEquals(Role.values.map(_.wire).toVector, Vector("user", "assistant"))

  test("implementation: name and version alone, then every optional field"):
    assertGolden(
      Golden.implementationMinimal,
      Sample.implementationMinimal,
      Implementation.decoder,
      Implementation.encoder
    )
    assertGolden(
      Golden.implementationFull,
      Sample.implementationFull,
      Implementation.decoder,
      Implementation.encoder
    )

  test("implementation: name and version are both required"):
    assertEquals(
      rendered("""{"name":"a"}""", Implementation.decoder),
      "$.version: missing required field"
    )
    assertEquals(
      rendered("""{"version":"1"}""", Implementation.decoder),
      "$.name: missing required field"
    )

  // --- capabilities --------------------------------------------------------------------------

  test("client capabilities: nothing declared is an empty object"):
    assertGolden("{}", ClientCapabilities.empty, ClientCapabilities.decoder, ClientCapabilities.encoder)

  test("client capabilities: the whole tree, nested markers included"):
    assertGolden(
      Golden.clientCapabilitiesFull,
      Sample.clientCapabilitiesFull,
      ClientCapabilities.decoder,
      ClientCapabilities.encoder
    )

  test("client capabilities: a declared-but-empty marker is not the same as an absent one"):
    // `sampling:{}` says the client samples; no `sampling` key says it does not.
    assertEquals(
      decoded("""{"sampling":{}}""", ClientCapabilities.decoder),
      ClientCapabilities(None, None, Some(SamplingCapability(None, None)), None, None)
    )
    assertEquals(decoded("""{}""", ClientCapabilities.decoder).sampling, None)
    assertEquals(
      Codec.encode(
        ClientCapabilities(None, None, Some(SamplingCapability(None, None)), None, None),
        ClientCapabilities.encoder
      ),
      """{"sampling":{}}"""
    )

  test("client capabilities: an object-typed marker keeps whatever is inside it"):
    assertEquals(
      decoded("""{"sampling":{"context":{"depth":2}}}""", ClientCapabilities.decoder).sampling,
      Some(SamplingCapability(Some(Json.obj("depth" -> Json.Num(2))), None))
    )

  test("server capabilities: nothing offered is an empty object"):
    assertGolden("{}", ServerCapabilities.empty, ServerCapabilities.decoder, ServerCapabilities.encoder)

  test("server capabilities: the whole tree"):
    assertGolden(
      Golden.serverCapabilitiesFull,
      Sample.serverCapabilitiesFull,
      ServerCapabilities.decoder,
      ServerCapabilities.encoder
    )

  test("server capabilities: a boolean where a marker object belongs is refused"):
    assertEquals(
      rendered("""{"logging":true}""", ServerCapabilities.decoder),
      "$.logging: expected an object, found boolean"
    )
    assertEquals(
      rendered("""{"resources":{"subscribe":"yes"}}""", ServerCapabilities.decoder),
      "$.resources.subscribe: expected a boolean, found string"
    )

  // --- initialize ----------------------------------------------------------------------------

  test("initialize params: the three required fields, then everything"):
    assertGolden(
      Golden.initializeMinimal,
      Sample.initializeMinimal,
      InitializeRequestParams.decoder,
      InitializeRequestParams.encoder
    )
    assertGolden(
      Golden.initializeFull,
      Sample.initializeFull,
      InitializeRequestParams.decoder,
      InitializeRequestParams.encoder
    )

  test("initialize params: each required field is named when it is missing"):
    assertEquals(
      rendered("""{"capabilities":{},"clientInfo":{"name":"a","version":"1"}}""", InitializeRequestParams.decoder),
      "$.protocolVersion: missing required field"
    )
    assertEquals(
      rendered("""{"protocolVersion":"2025-11-25","clientInfo":{"name":"a","version":"1"}}""", InitializeRequestParams.decoder),
      "$.capabilities: missing required field"
    )
    assertEquals(
      rendered("""{"protocolVersion":"2025-11-25","capabilities":{}}""", InitializeRequestParams.decoder),
      "$.clientInfo: missing required field"
    )

  test("initialize params: the protocol version is a string, not a checked constant"):
    // A client may ask for an older revision; refusing to decode it would make the version
    // negotiation the schema describes impossible.
    assertEquals(
      decoded(
        """{"protocolVersion":"2024-11-05","capabilities":{},"clientInfo":{"name":"a","version":"1"}}""",
        InitializeRequestParams.decoder
      ).protocolVersion,
      "2024-11-05"
    )
    assertEquals(Mcp.LatestProtocolVersion, "2025-11-25")

  test("initialize result: the three required fields, then everything"):
    assertGolden(
      Golden.initializeResultMinimal,
      Sample.initializeResultMinimal,
      InitializeResult.decoder,
      InitializeResult.encoder
    )
    assertGolden(
      Golden.initializeResultFull,
      Sample.initializeResultFull,
      InitializeResult.decoder,
      InitializeResult.encoder
    )

  test("initialize result: serverInfo is required and instructions are not"):
    assertEquals(
      rendered("""{"protocolVersion":"2025-11-25","capabilities":{}}""", InitializeResult.decoder),
      "$.serverInfo: missing required field"
    )
    assertEquals(
      decoded(
        """{"protocolVersion":"2025-11-25","capabilities":{},"serverInfo":{"name":"a","version":"1"},"instructions":null}""",
        InitializeResult.decoder
      ).instructions,
      None
    )
