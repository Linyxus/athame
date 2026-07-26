package mcp

/** Cancellation, progress, the empty result and the task-augmentation records
  * (schema.ts:201-247, 570-619, 1313-1337).
  */
class UtilitySuite extends McpSuite:

  test("cancelled params: everything is optional, so an empty object is a whole notification"):
    assertGolden(
      Golden.cancelledMinimal,
      Sample.cancelledMinimal,
      CancelledNotificationParams.decoder,
      CancelledNotificationParams.encoder
    )
    assertGolden(
      Golden.cancelledFull,
      Sample.cancelledFull,
      CancelledNotificationParams.decoder,
      CancelledNotificationParams.encoder
    )

  test("cancelled params: the request id keeps whichever spelling it arrived in"):
    assertEquals(
      decoded("""{"requestId":"abc"}""", CancelledNotificationParams.decoder).requestId,
      Some(RequestId.Str("abc"))
    )
    assertEquals(
      decoded("""{"requestId":7}""", CancelledNotificationParams.decoder).requestId,
      Some(RequestId.Num(7))
    )
    assertEquals(
      rendered("""{"requestId":[]}""", CancelledNotificationParams.decoder),
      "$.requestId: expected a string or number, found array"
    )

  test("progress params: the token and the count, then the optional rest"):
    assertGolden(
      Golden.progressMinimal,
      Sample.progressMinimal,
      ProgressNotificationParams.decoder,
      ProgressNotificationParams.encoder
    )
    assertGolden(
      Golden.progressFull,
      Sample.progressFull,
      ProgressNotificationParams.decoder,
      ProgressNotificationParams.encoder
    )

  test("progress params: token and progress are both required"):
    assertEquals(
      rendered("""{"progress":1}""", ProgressNotificationParams.decoder),
      "$.progressToken: missing required field"
    )
    assertEquals(
      rendered("""{"progressToken":"p"}""", ProgressNotificationParams.decoder),
      "$.progress: missing required field"
    )

  test("progress params: an integral count encodes without a decimal point"):
    assertEquals(
      Codec.encode(
        ProgressNotificationParams(None, ProgressToken.Num(1), 3, Some(10), None),
        ProgressNotificationParams.encoder
      ),
      """{"progressToken":1,"progress":3,"total":10}"""
    )

  test("progress: the token a request asked with is the token the update comes back with"):
    // The round trip the two halves of the protocol make together: into a request's _meta, out of a
    // progress notification's own field.
    val token = ProgressToken.Str("p-9")
    val params = RequestParams(Some(ProgressToken.into(None, token)))
    assertEquals(Codec.encode(params, RequestParams.encoder), """{"_meta":{"progressToken":"p-9"}}""")
    val update = ProgressNotificationParams(None, ProgressToken.from(params.meta).get, 1, None, None)
    assertEquals(update.progressToken, token)

  test("empty result: nothing, or nothing plus meta"):
    assertGolden(Golden.emptyResult, EmptyResult.empty, EmptyResult.decoder, EmptyResult.encoder)
    assertGolden(
      s"""{"_meta":${Golden.meta}}""",
      EmptyResult(Some(Sample.meta)),
      EmptyResult.decoder,
      EmptyResult.encoder
    )

  test("empty result: extra members are ignored rather than kept"):
    // Result's index signature is deliberately not modelled: what we do not know, we drop.
    assertEquals(decoded("""{"unexpected":[1,2]}""", EmptyResult.decoder), EmptyResult.empty)

  test("request and notification params: a _meta and nothing else"):
    assertGolden("{}", RequestParams.empty, RequestParams.decoder, RequestParams.encoder)
    assertGolden("{}", NotificationParams.empty, NotificationParams.decoder, NotificationParams.encoder)
    assertGolden(
      s"""{"_meta":${Golden.meta}}""",
      RequestParams(Some(Sample.meta)),
      RequestParams.decoder,
      RequestParams.encoder
    )

  test("meta: a _meta of any shape is carried verbatim, order included"):
    val text = """{"_meta":{"z":1,"a":{"deep":[null,true]},"m":"s"}}"""
    assertEquals(Codec.encode(decoded(text, RequestParams.decoder), RequestParams.encoder), text)

  test("meta: a _meta that is not an object is refused"):
    assertEquals(rendered("""{"_meta":[]}""", RequestParams.decoder), "$._meta: expected an object, found array")

  test("task metadata: the ttl, and the empty request for a task"):
    assertGolden("""{"ttl":60000}""", TaskMetadata(Some(60000)), TaskMetadata.decoder, TaskMetadata.encoder)
    assertGolden("{}", TaskMetadata(None), TaskMetadata.decoder, TaskMetadata.encoder)

  test("related task metadata: the record, and the well-known key it lives under"):
    assertEquals(RelatedTaskMetadata.MetaKey, "io.modelcontextprotocol/related-task")
    assertGolden(
      """{"taskId":"t-1"}""",
      RelatedTaskMetadata("t-1"),
      RelatedTaskMetadata.decoder,
      RelatedTaskMetadata.encoder
    )
    assertEquals(rendered("{}", RelatedTaskMetadata.decoder), "$.taskId: missing required field")

  test("related task metadata: it reads back out of a _meta the codecs carried"):
    val carried = decoded(s"""{"_meta":${Golden.meta}}""", RequestParams.decoder)
    val related = carried.meta.flatMap(_.get(RelatedTaskMetadata.MetaKey))
    assertEquals(related, Some(Json.obj("taskId" -> Json.Str("t-1"))))
    assertEquals(
      related.map(value => Codec.decode(Json.stringify(value), RelatedTaskMetadata.decoder)),
      Some(Right(RelatedTaskMetadata("t-1")))
    )

  test("error object: code and message required, data optional and opaque"):
    assertGolden(
      """{"code":-32601,"message":"Method not found"}""",
      Sample.errorMinimal,
      JsonRpc.Error.decoder,
      JsonRpc.Error.encoder
    )
    assertGolden(
      """{"code":-32602,"message":"Invalid params","data":{"field":"name"}}""",
      Sample.errorFull,
      JsonRpc.Error.decoder,
      JsonRpc.Error.encoder
    )

  test("error object: the codes MCP names"):
    assertEquals(
      Vector(
        ErrorCode.ParseError,
        ErrorCode.InvalidRequest,
        ErrorCode.MethodNotFound,
        ErrorCode.InvalidParams,
        ErrorCode.InternalError,
        ErrorCode.UrlElicitationRequired
      ),
      Vector(-32700.0, -32600.0, -32601.0, -32602.0, -32603.0, -32042.0)
    )

  test("error object: a missing code or message is named"):
    assertEquals(rendered("""{"message":"x"}""", JsonRpc.Error.decoder), "$.code: missing required field")
    assertEquals(rendered("""{"code":1}""", JsonRpc.Error.decoder), "$.message: missing required field")

  test("error object: data may be any JSON, null included, and null means absent"):
    assertEquals(decoded("""{"code":1,"message":"x","data":null}""", JsonRpc.Error.decoder).data, None)
    assertEquals(
      decoded("""{"code":1,"message":"x","data":[1,"a"]}""", JsonRpc.Error.decoder).data,
      Some(Json.arr(Json.Num(1), Json.Str("a")))
    )
