package mcp

/** `sampling/createMessage` (schema.ts:1572-1698, 1910-1992).
  *
  * The interesting rule here is `content`: one block or an array of them on the wire, always a
  * `Vector` in Scala, always an array on the way out. That is the single normalisation in the
  * package, so both wire forms are pinned and so is the shape they leave in.
  */
class SamplingSuite extends McpSuite:

  // --- the small records ---------------------------------------------------------------------

  test("include context: three spellings, none of them the default"):
    assertEquals(IncludeContext.values.map(_.wire).toVector, Vector("none", "thisServer", "allServers"))
    for value <- IncludeContext.values do
      assertGolden(s""""${value.wire}"""", value, IncludeContext.decoder, IncludeContext.encoder)
    assertEquals(
      rendered("\"everything\"", IncludeContext.decoder),
      """$: expected one of "none", "thisServer", "allServers", found "everything""""
    )

  test("tool choice: the three modes, and an absent one"):
    assertEquals(ToolChoiceMode.values.map(_.wire).toVector, Vector("auto", "required", "none"))
    for mode <- ToolChoiceMode.values do
      assertGolden(s"""{"mode":"${mode.wire}"}""", ToolChoice(Some(mode)), ToolChoice.decoder, ToolChoice.encoder)
    assertGolden("{}", ToolChoice(None), ToolChoice.decoder, ToolChoice.encoder)

  test("model preferences: hints and the three priorities"):
    assertGolden("{}", ModelPreferences(None, None, None, None), ModelPreferences.decoder, ModelPreferences.encoder)
    assertGolden(
      """{"hints":[{"name":"sonnet"},{}],"costPriority":0.1,"speedPriority":0.2,"intelligencePriority":0.9}""",
      ModelPreferences(Some(Vector(ModelHint(Some("sonnet")), ModelHint(None))), Some(0.1), Some(0.2), Some(0.9)),
      ModelPreferences.decoder,
      ModelPreferences.encoder
    )

  test("model preferences: an out-of-range priority is carried, not refused"):
    // The bounds are advisory in the schema and the client may ignore the whole block; throwing
    // away a message over one would be a worse failure than honouring a 2.
    assertEquals(decoded("""{"costPriority":2}""", ModelPreferences.decoder).costPriority, Some(2.0))

  test("stop reason: an open union, so the four names are constants and not a type"):
    assertEquals(
      Vector(StopReason.EndTurn, StopReason.StopSequence, StopReason.MaxTokens, StopReason.ToolUse),
      Vector("endTurn", "stopSequence", "maxTokens", "toolUse")
    )
    assertEquals(
      decoded(s"""{"role":"user","content":[],"model":"m","stopReason":"provider_specific"}""", CreateMessageResult.decoder).stopReason,
      Some("provider_specific")
    )

  // --- messages ------------------------------------------------------------------------------

  test("sampling message: role and content, with meta optional"):
    assertGolden(Golden.samplingMessage, Sample.samplingMessage, SamplingMessage.decoder, SamplingMessage.encoder)
    assertGolden(
      Golden.samplingMessageFull,
      Sample.samplingMessageFull,
      SamplingMessage.decoder,
      SamplingMessage.encoder
    )

  test("sampling message: a single block decodes as a vector of one"):
    // The schema allows `block | block[]`; both mean one message with one block.
    assertEquals(
      decoded(s"""{"role":"user","content":${Golden.textMinimal}}""", SamplingMessage.decoder),
      SamplingMessage(Role.User, Vector(Sample.textMinimal), None)
    )

  test("sampling message: the array form is what leaves, whichever form arrived"):
    val single = s"""{"role":"user","content":${Golden.textMinimal}}"""
    val array = s"""{"role":"user","content":[${Golden.textMinimal}]}"""
    assertEquals(Codec.encode(decoded(single, SamplingMessage.decoder), SamplingMessage.encoder), array)
    assertEquals(Codec.encode(decoded(array, SamplingMessage.decoder), SamplingMessage.encoder), array)
    // Same value from both spellings, which is the property the round-trip law is about.
    assertEquals(decoded(single, SamplingMessage.decoder), decoded(array, SamplingMessage.decoder))

  test("sampling message: a broken single block reports without an index"):
    assertEquals(
      rendered("""{"role":"user","content":{"type":"nope"}}""", SamplingMessage.decoder),
      """$.content.type: expected one of "text", "image", "audio", "tool_use", "tool_result", found "nope""""
    )

  test("sampling message: a broken block inside an array reports with one"):
    assertEquals(
      rendered("""{"role":"user","content":[{"type":"text","text":"a"},{"type":"nope"}]}""", SamplingMessage.decoder),
      """$.content[1].type: expected one of "text", "image", "audio", "tool_use", "tool_result", found "nope""""
    )

  test("sampling message: role is required and closed"):
    assertEquals(rendered("""{"content":[]}""", SamplingMessage.decoder), "$.role: missing required field")
    assertEquals(
      rendered("""{"role":"system","content":[]}""", SamplingMessage.decoder),
      """$.role: expected one of "user", "assistant", found "system""""
    )

  // --- the request ---------------------------------------------------------------------------

  test("createMessage params: messages and maxTokens alone, then everything"):
    assertGolden(
      Golden.createMessageMinimal,
      Sample.createMessageMinimal,
      CreateMessageRequestParams.decoder,
      CreateMessageRequestParams.encoder
    )
    assertGolden(
      Golden.createMessageFull,
      Sample.createMessageFull,
      CreateMessageRequestParams.decoder,
      CreateMessageRequestParams.encoder
    )

  test("createMessage params: messages and maxTokens are the required pair"):
    assertEquals(
      rendered("""{"maxTokens":1}""", CreateMessageRequestParams.decoder),
      "$.messages: missing required field"
    )
    assertEquals(
      rendered("""{"messages":[]}""", CreateMessageRequestParams.decoder),
      "$.maxTokens: missing required field"
    )

  test("createMessage params: a bad tool deep in the request is located exactly"):
    // The path contract from the design note, end to end.
    val text =
      s"""{"messages":[],"maxTokens":1,"tools":[${Golden.toolMinimal},${Golden.toolMinimal},{"name":5,"inputSchema":{"type":"object"}}]}"""
    assertEquals(rendered(text, CreateMessageRequestParams.decoder), "$.tools[2].name: expected a string, found number")

  test("createMessage params: metadata is opaque and survives whole"):
    val text = """{"messages":[],"maxTokens":1,"metadata":{"z":[1,{"a":null}],"a":"b"}}"""
    assertEquals(
      Codec.encode(decoded(text, CreateMessageRequestParams.decoder), CreateMessageRequestParams.encoder),
      text
    )

  // --- the result ----------------------------------------------------------------------------

  test("createMessage result: role, content and model, with the rest optional"):
    assertGolden(
      Golden.createMessageResultMinimal,
      Sample.createMessageResultMinimal,
      CreateMessageResult.decoder,
      CreateMessageResult.encoder
    )
    assertGolden(
      Golden.createMessageResultFull,
      Sample.createMessageResultFull,
      CreateMessageResult.decoder,
      CreateMessageResult.encoder
    )

  test("createMessage result: it takes the single-block form too"):
    assertEquals(
      decoded(s"""{"role":"assistant","content":${Golden.textMinimal},"model":"m"}""", CreateMessageResult.decoder),
      CreateMessageResult(None, Role.Assistant, Vector(Sample.textMinimal), "m", None)
    )

  test("createMessage result: model is required"):
    assertEquals(
      rendered("""{"role":"assistant","content":[]}""", CreateMessageResult.decoder),
      "$.model: missing required field"
    )

  test("createMessage result: the two _meta the schema gives it are one field"):
    // It extends both Result and SamplingMessage, each declaring _meta; there is one on the wire.
    assertEquals(
      decoded(s"""{"_meta":${Golden.meta},"role":"assistant","content":[],"model":"m"}""", CreateMessageResult.decoder).meta,
      Some(Sample.meta)
    )
