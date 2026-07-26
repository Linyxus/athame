package mcp

/** Tools: definitions, listing and calls (schema.ts:1080-1297). */
class ToolsSuite extends McpSuite:

  // --- the schema carrier --------------------------------------------------------------------

  test("json schema: type is pinned to object and written back, not stored"):
    assertGolden(
      """{"type":"object"}""",
      JsonSchemaObject.empty,
      JsonSchemaObject.decoder,
      JsonSchemaObject.encoder
    )
    assertGolden(Golden.inputSchema, Sample.inputSchema, JsonSchemaObject.decoder, JsonSchemaObject.encoder)

  test("json schema: a type other than object is refused"):
    assertEquals(
      rendered("""{"type":"array"}""", JsonSchemaObject.decoder),
      """$.type: expected "object", found "array""""
    )
    assertEquals(rendered("""{}""", JsonSchemaObject.decoder), "$.type: missing required field")

  test("json schema: properties are carried verbatim, however deep"):
    // We transport schemas; we do not interpret them, so a draft we have never seen must survive.
    val nested =
      """{"type":"object","properties":{"a":{"type":"array","items":{"$ref":"#/x"},"weird":[1,{"z":null}]}}}"""
    assertEquals(Codec.encode(decoded(nested, JsonSchemaObject.decoder), JsonSchemaObject.encoder), nested)

  // --- annotations and execution -------------------------------------------------------------

  test("tool annotations: all four hints, and none of them"):
    assertGolden("{}", ToolAnnotations(None, None, None, None, None), ToolAnnotations.decoder, ToolAnnotations.encoder)
    assertGolden(
      """{"title":"Search","readOnlyHint":true,"destructiveHint":false,"idempotentHint":true,"openWorldHint":false}""",
      ToolAnnotations(Some("Search"), Some(true), Some(false), Some(true), Some(false)),
      ToolAnnotations.decoder,
      ToolAnnotations.encoder
    )

  test("tool execution: the three task-support spellings"):
    assertEquals(TaskSupport.values.map(_.wire).toVector, Vector("forbidden", "optional", "required"))
    for support <- TaskSupport.values do
      assertGolden(
        s"""{"taskSupport":"${support.wire}"}""",
        ToolExecution(Some(support)),
        ToolExecution.decoder,
        ToolExecution.encoder
      )
    assertGolden("{}", ToolExecution(None), ToolExecution.decoder, ToolExecution.encoder)

  test("tool execution: an unknown task-support value is named"):
    assertEquals(
      rendered("""{"taskSupport":"maybe"}""", ToolExecution.decoder),
      """$.taskSupport: expected one of "forbidden", "optional", "required", found "maybe""""
    )

  // --- tools ---------------------------------------------------------------------------------

  test("tool: name and inputSchema alone, then every optional field"):
    assertGolden(Golden.toolMinimal, Sample.toolMinimal, Tool.decoder, Tool.encoder)
    assertGolden(Golden.toolFull, Sample.toolFull, Tool.decoder, Tool.encoder)

  test("tool: name and inputSchema are the two required fields"):
    assertEquals(
      rendered("""{"inputSchema":{"type":"object"}}""", Tool.decoder),
      "$.name: missing required field"
    )
    assertEquals(rendered("""{"name":"a"}""", Tool.decoder), "$.inputSchema: missing required field")

  test("tool: a broken inputSchema reports where inside it broke"):
    assertEquals(
      rendered("""{"name":"a","inputSchema":{"type":"object","required":"q"}}""", Tool.decoder),
      "$.inputSchema.required: expected an array, found string"
    )

  // --- listing -------------------------------------------------------------------------------

  test("tools/list result: an empty page, and a page with a cursor"):
    assertGolden(Golden.listToolsMinimal, Sample.listToolsMinimal, ListToolsResult.decoder, ListToolsResult.encoder)
    assertGolden(Golden.listToolsFull, Sample.listToolsFull, ListToolsResult.decoder, ListToolsResult.encoder)

  test("tools/list result: tools is required, and a bad one is located by index"):
    assertEquals(rendered("""{}""", ListToolsResult.decoder), "$.tools: missing required field")
    assertEquals(
      rendered(
        s"""{"tools":[${Golden.toolMinimal},{"name":5,"inputSchema":{"type":"object"}}]}""",
        ListToolsResult.decoder
      ),
      "$.tools[1].name: expected a string, found number"
    )

  test("pagination: the cursor is carried on the way out and back"):
    assertGolden(
      """{"cursor":"abc"}""",
      PaginatedRequestParams(None, Some("abc")),
      PaginatedRequestParams.decoder,
      PaginatedRequestParams.encoder
    )
    assertGolden("{}", PaginatedRequestParams.empty, PaginatedRequestParams.decoder, PaginatedRequestParams.encoder)

  // --- calling -------------------------------------------------------------------------------

  test("tools/call params: a bare call, and one with meta, task and arguments"):
    assertGolden(
      Golden.callToolMinimal,
      Sample.callToolMinimal,
      CallToolRequestParams.decoder,
      CallToolRequestParams.encoder
    )
    assertGolden(
      Golden.callToolFull,
      Sample.callToolFull,
      CallToolRequestParams.decoder,
      CallToolRequestParams.encoder
    )

  test("tools/call params: arguments are arbitrary JSON and keep their order"):
    val ordered = """{"name":"t","arguments":{"z":1,"a":[null,true],"m":{"n":"s"}}}"""
    assertEquals(
      Codec.encode(decoded(ordered, CallToolRequestParams.decoder), CallToolRequestParams.encoder),
      ordered
    )

  test("tools/call params: name is required, arguments must be an object"):
    assertEquals(rendered("""{}""", CallToolRequestParams.decoder), "$.name: missing required field")
    assertEquals(
      rendered("""{"name":"t","arguments":[]}""", CallToolRequestParams.decoder),
      "$.arguments: expected an object, found array"
    )

  test("tools/call params: task augmentation survives, ttl and all"):
    assertEquals(
      decoded("""{"name":"t","task":{"ttl":5000}}""", CallToolRequestParams.decoder).task,
      Some(TaskMetadata(Some(5000)))
    )
    assertEquals(
      decoded("""{"name":"t","task":{}}""", CallToolRequestParams.decoder).task,
      Some(TaskMetadata(None))
    )
    // Absent is not the same as present-and-empty: one asks for a task, the other does not.
    assertEquals(decoded("""{"name":"t"}""", CallToolRequestParams.decoder).task, None)

  test("tools/call result: an empty result, and one with everything"):
    assertGolden(
      Golden.callToolResultMinimal,
      Sample.callToolResultMinimal,
      CallToolResult.decoder,
      CallToolResult.encoder
    )
    assertGolden(
      Golden.callToolResultFull,
      Sample.callToolResultFull,
      CallToolResult.decoder,
      CallToolResult.encoder
    )

  test("tools/call result: content is required, isError is not"):
    assertEquals(rendered("""{}""", CallToolResult.decoder), "$.content: missing required field")
    assertEquals(decoded("""{"content":[]}""", CallToolResult.decoder).isError, None)
    assertEquals(decoded("""{"content":[],"isError":false}""", CallToolResult.decoder).isError, Some(false))

  test("tools/call result: a broken content block is located by index"):
    assertEquals(
      rendered("""{"content":[{"type":"text","text":"a"},{"type":"image","data":"d"}]}""", CallToolResult.decoder),
      "$.content[1].mimeType: missing required field"
    )
