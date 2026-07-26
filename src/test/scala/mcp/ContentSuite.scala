package mcp

/** Content blocks, resources and resource contents (schema.ts:797-919, 1044-1069, 1737-1908).
  *
  * Two unions that overlap, both discriminated by `type`, plus a third — [[ResourceContents]] —
  * that has no discriminator at all and is told apart by which of `text` and `blob` is present.
  * Getting any of those rules backwards would silently change what a model is shown, so each
  * discriminator is pinned from both directions.
  */
class ContentSuite extends McpSuite:

  // --- the shared records --------------------------------------------------------------------

  test("annotations: audience, priority and lastModified, present and absent"):
    assertGolden(Golden.annotationsFull, Sample.annotationsFull, Annotations.decoder, Annotations.encoder)
    assertGolden("{}", Sample.annotationsEmpty, Annotations.decoder, Annotations.encoder)

  test("annotations: the audience is a closed union of roles"):
    assertEquals(
      rendered("""{"audience":["user","robot"]}""", Annotations.decoder),
      """$.audience[1]: expected one of "user", "assistant", found "robot""""
    )

  // --- text, image, audio --------------------------------------------------------------------

  test("text content: with and without its optional halves"):
    assertGolden(Golden.textMinimal, Sample.textMinimal, TextContent.decoder, TextContent.encoder)
    assertGolden(Golden.textFull, Sample.textFull, TextContent.decoder, TextContent.encoder)

  test("image content: data and mimeType are both required"):
    assertGolden(Golden.image, Sample.image, ImageContent.decoder, ImageContent.encoder)
    assertEquals(
      rendered("""{"type":"image","data":"aGk="}""", ImageContent.decoder),
      "$.mimeType: missing required field"
    )
    assertEquals(
      rendered("""{"type":"image","mimeType":"image/png"}""", ImageContent.decoder),
      "$.data: missing required field"
    )

  test("audio content: same shape as image, different tag"):
    assertGolden(Golden.audio, Sample.audio, AudioContent.decoder, AudioContent.encoder)

  test("content: a block's own decoder insists on its own tag"):
    assertEquals(
      rendered(Golden.image, TextContent.decoder),
      """$.type: expected "text", found "image""""
    )

  // --- resources -----------------------------------------------------------------------------

  test("resource: the record ResourceLink is built from"):
    assertGolden(Golden.resourceMinimal, Sample.resourceMinimal, Resource.decoder, Resource.encoder)
    assertGolden(Golden.resourceFull, Sample.resourceFull, Resource.decoder, Resource.encoder)

  test("resource link: a resource with a type in front of it"):
    assertGolden(Golden.resourceLink, Sample.resourceLink, ResourceLink.decoder, ResourceLink.encoder)
    assertGolden(
      Golden.resourceLinkMinimal,
      Sample.resourceLinkMinimal,
      ResourceLink.decoder,
      ResourceLink.encoder
    )

  test("resource link: it and the resource it wraps carry the same fields"):
    assertEquals(Sample.resourceLink.resource, Sample.resourceFull)
    assertEquals(ResourceLink(Sample.resourceFull), Sample.resourceLink)

  test("resource contents: text and blob, told apart by which one is there"):
    assertGolden(
      Golden.textContents,
      Sample.textContents,
      TextResourceContents.decoder,
      TextResourceContents.encoder
    )
    assertGolden(
      Golden.blobContents,
      Sample.blobContents,
      BlobResourceContents.decoder,
      BlobResourceContents.encoder
    )
    assertEquals(decoded(Golden.textContents, ResourceContents.decoder), Sample.textContents)
    assertEquals(decoded(Golden.blobContents, ResourceContents.decoder), Sample.blobContents)

  test("resource contents: neither text nor blob is a refusal, not a guess"):
    assertEquals(
      rendered("""{"uri":"file:///x"}""", ResourceContents.decoder),
      """$: expected an object with "text" or "blob", found object"""
    )
    assertEquals(
      rendered("""{"uri":"file:///x","text":null,"blob":null}""", ResourceContents.decoder),
      """$: expected an object with "text" or "blob", found object"""
    )

  test("resource contents: both present reads as text, the documented tie-break"):
    assertEquals(
      decoded("""{"uri":"file:///x","text":"t","blob":"b"}""", ResourceContents.decoder),
      TextResourceContents("file:///x", None, None, "t")
    )

  test("embedded resource: carries either kind of contents"):
    assertGolden(
      Golden.embeddedText,
      Sample.embeddedText,
      EmbeddedResource.decoder,
      EmbeddedResource.encoder
    )
    assertGolden(
      Golden.embeddedBlob,
      Sample.embeddedBlob,
      EmbeddedResource.decoder,
      EmbeddedResource.encoder
    )

  // --- tool use and tool result --------------------------------------------------------------

  test("tool use: id, name and input are all required"):
    assertGolden(Golden.toolUse, Sample.toolUse, ToolUseContent.decoder, ToolUseContent.encoder)
    assertEquals(
      rendered("""{"type":"tool_use","id":"a","name":"b"}""", ToolUseContent.decoder),
      "$.input: missing required field"
    )
    assertEquals(
      rendered("""{"type":"tool_use","id":"a","name":"b","input":[]}""", ToolUseContent.decoder),
      "$.input: expected an object, found array"
    )

  test("tool result: nests content blocks, and the path says where"):
    assertGolden(
      Golden.toolResult,
      Sample.toolResult,
      ToolResultContent.decoder,
      ToolResultContent.encoder
    )
    assertEquals(
      rendered(
        """{"type":"tool_result","toolUseId":"a","content":[{"type":"text","text":"x"},{"type":"text"}]}""",
        ToolResultContent.decoder
      ),
      "$.content[1].text: missing required field"
    )

  // --- the unions ----------------------------------------------------------------------------

  test("content block: every member decodes through the union"):
    val blocks = decoded(Golden.contentBlocks, Decode.vector(ContentBlock.decoder))
    assertEquals(blocks, Sample.contentBlocks)
    assertEquals(Codec.encode(Sample.contentBlocks, Encode.vector(ContentBlock.encoder)), Golden.contentBlocks)

  test("content block: the tags are the ones the schema names, in its order"):
    assertEquals(
      ContentBlock.Tags,
      Vector("text", "image", "audio", "resource_link", "resource")
    )
    assertEquals(Sample.contentBlocks.map(_.tag).distinct, ContentBlock.Tags)

  test("content block: an unknown type is named, not silently read as text"):
    assertEquals(
      rendered("""{"type":"video","data":"x"}""", ContentBlock.decoder),
      """$.type: expected one of "text", "image", "audio", "resource_link", "resource", found "video""""
    )

  test("content block: tool_use and tool_result are not content blocks"):
    // They belong to the sampling union only; admitting them here would let a tool result claim to
    // be a prompt.
    assertEquals(
      rendered(Golden.toolUse, ContentBlock.decoder),
      """$.type: expected one of "text", "image", "audio", "resource_link", "resource", found "tool_use""""
    )

  test("content block: a missing type is a missing field, not an unknown one"):
    assertEquals(rendered("""{"text":"x"}""", ContentBlock.decoder), "$.type: missing required field")

  test("sampling block: every member decodes through the union"):
    val blocks = decoded(Golden.samplingBlocks, Decode.vector(SamplingMessageContentBlock.decoder))
    assertEquals(blocks, Sample.samplingBlocks)
    assertEquals(
      Codec.encode(Sample.samplingBlocks, Encode.vector(SamplingMessageContentBlock.encoder)),
      Golden.samplingBlocks
    )

  test("sampling block: the tags are the ones the schema names, in its order"):
    assertEquals(
      SamplingMessageContentBlock.Tags,
      Vector("text", "image", "audio", "tool_use", "tool_result")
    )

  test("sampling block: resource_link and resource are not sampling blocks"):
    assertEquals(
      rendered(Golden.resourceLinkMinimal, SamplingMessageContentBlock.decoder),
      """$.type: expected one of "text", "image", "audio", "tool_use", "tool_result", found "resource_link""""
    )

  test("content: the three shared members sit in both unions and mean the same thing"):
    for shared <- Vector(Golden.textFull, Golden.image, Golden.audio) do
      assertEquals(
        decoded(shared, ContentBlock.decoder).tag,
        decoded(shared, SamplingMessageContentBlock.decoder).tag
      )
