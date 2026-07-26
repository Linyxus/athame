package mcp

/** `elicitation/create` and the schema family behind it (schema.ts:2150-2492).
  *
  * Eight members of [[PrimitiveSchemaDefinition]], five of which say `type: "string"`. The
  * discrimination rule the package documents is spelled out again here as tests, because a rule
  * that lives only in a comment is a rule that drifts.
  */
class ElicitationSuite extends McpSuite:

  // --- the eight schema members --------------------------------------------------------------

  test("string schema: bare, and with every constraint"):
    assertGolden(Golden.stringSchemaMinimal, Sample.stringSchemaMinimal, StringSchema.decoder, StringSchema.encoder)
    assertGolden(Golden.stringSchema, Sample.stringSchema, StringSchema.decoder, StringSchema.encoder)

  test("string schema: the four formats, and nothing else"):
    assertEquals(StringFormat.values.map(_.wire).toVector, Vector("email", "uri", "date", "date-time"))
    for format <- StringFormat.values do
      assertEquals(
        decoded(s"""{"type":"string","format":"${format.wire}"}""", StringSchema.decoder).format,
        Some(format)
      )
    assertEquals(
      rendered("""{"type":"string","format":"hostname"}""", StringSchema.decoder),
      """$.format: expected one of "email", "uri", "date", "date-time", found "hostname""""
    )

  test("number schema: number and integer are different, and each stays what it was"):
    assertGolden(Golden.numberSchema, Sample.numberSchema, NumberSchema.decoder, NumberSchema.encoder)
    assertGolden(Golden.integerSchema, Sample.integerSchema, NumberSchema.decoder, NumberSchema.encoder)
    assertEquals(Sample.numberSchema.schemaType, "number")
    assertEquals(Sample.integerSchema.schemaType, "integer")

  test("boolean schema: title, description and default"):
    assertGolden(Golden.booleanSchema, Sample.booleanSchema, BooleanSchema.decoder, BooleanSchema.encoder)
    assertGolden("""{"type":"boolean"}""", BooleanSchema(None, None, None), BooleanSchema.decoder, BooleanSchema.encoder)

  test("enum schemas: all five of them, each golden in both directions"):
    assertGolden(
      Golden.untitledSingle,
      Sample.untitledSingle,
      UntitledSingleSelectEnumSchema.decoder,
      UntitledSingleSelectEnumSchema.encoder
    )
    assertGolden(
      Golden.titledSingle,
      Sample.titledSingle,
      TitledSingleSelectEnumSchema.decoder,
      TitledSingleSelectEnumSchema.encoder
    )
    assertGolden(
      Golden.legacyTitled,
      Sample.legacyTitled,
      LegacyTitledEnumSchema.decoder,
      LegacyTitledEnumSchema.encoder
    )
    assertGolden(
      Golden.untitledMulti,
      Sample.untitledMulti,
      UntitledMultiSelectEnumSchema.decoder,
      UntitledMultiSelectEnumSchema.encoder
    )
    assertGolden(
      Golden.titledMulti,
      Sample.titledMulti,
      TitledMultiSelectEnumSchema.decoder,
      TitledMultiSelectEnumSchema.encoder
    )

  // --- the discrimination rule, one test per clause ------------------------------------------

  test("discrimination: type boolean is a boolean schema"):
    assertEquals(decoded("""{"type":"boolean"}""", PrimitiveSchemaDefinition.decoder), BooleanSchema(None, None, None))

  test("discrimination: type number and type integer are both number schemas"):
    assertEquals(
      decoded("""{"type":"number"}""", PrimitiveSchemaDefinition.decoder),
      NumberSchema(NumberType.Number, None, None, None, None, None)
    )
    assertEquals(
      decoded("""{"type":"integer"}""", PrimitiveSchemaDefinition.decoder),
      NumberSchema(NumberType.Integer, None, None, None, None, None)
    )

  test("discrimination: type string with oneOf is the titled single-select"):
    assertEquals(
      decoded("""{"type":"string","oneOf":[{"const":"a","title":"A"}]}""", PrimitiveSchemaDefinition.decoder),
      TitledSingleSelectEnumSchema(None, None, Vector(EnumOption("a", "A")), None)
    )

  test("discrimination: type string with enum and enumNames is the legacy schema"):
    // Ruling M10, second required test: a Legacy vector carrying enumNames, golden in both
    // directions via Golden.legacyTitled above and decoded through the union here.
    assertEquals(
      decoded("""{"type":"string","enum":["a"],"enumNames":["A"]}""", PrimitiveSchemaDefinition.decoder),
      LegacyTitledEnumSchema(None, None, Vector("a"), Vector("A"), None)
    )

  test("discrimination: type string with enum but no enumNames is the untitled single-select"):
    // Ruling M10, first required test: the bare-enum wire form. The schema marks enumNames
    // optional, which makes Legacy and Untitled byte-identical without it, so the rule picks the
    // member the schema has not deprecated. LegacyTitledEnumSchema requires enumNames in Scala for
    // exactly this reason — the model is the quotient of the wire language by this table, so every
    // value it can hold is one this rule can recover.
    assertEquals(
      decoded("""{"type":"string","enum":["a"]}""", PrimitiveSchemaDefinition.decoder),
      UntitledSingleSelectEnumSchema(None, None, Vector("a"), None)
    )

  test("discrimination: type string with neither is a plain string schema"):
    assertEquals(
      decoded("""{"type":"string","minLength":2}""", PrimitiveSchemaDefinition.decoder),
      StringSchema(None, None, Some(2), None, None, None)
    )

  test("discrimination: type array with items.anyOf is the titled multi-select"):
    assertEquals(
      decoded("""{"type":"array","items":{"anyOf":[{"const":"a","title":"A"}]}}""", PrimitiveSchemaDefinition.decoder),
      TitledMultiSelectEnumSchema(None, None, None, None, TitledMultiSelectItems(Vector(EnumOption("a", "A"))), None)
    )

  test("discrimination: type array without items.anyOf is the untitled multi-select"):
    assertEquals(
      decoded("""{"type":"array","items":{"type":"string","enum":["a"]}}""", PrimitiveSchemaDefinition.decoder),
      UntitledMultiSelectEnumSchema(None, None, None, None, UntitledMultiSelectItems(Vector("a")), None)
    )

  test("discrimination: oneOf wins over enum when a sender supplies both"):
    // First clause of the rule, made visible. Nothing legal produces this, so the point is only
    // that the order is fixed rather than incidental.
    assertEquals(
      decoded("""{"type":"string","oneOf":[{"const":"a","title":"A"}],"enum":["a"]}""", PrimitiveSchemaDefinition.decoder),
      TitledSingleSelectEnumSchema(None, None, Vector(EnumOption("a", "A")), None)
    )

  test("discrimination: a null discriminator counts as absent, like every other field"):
    assertEquals(
      decoded("""{"type":"string","oneOf":null,"enum":null}""", PrimitiveSchemaDefinition.decoder),
      StringSchema(None, None, None, None, None, None)
    )

  test("discrimination: an unknown type is named, with the five it could have been"):
    assertEquals(
      rendered("""{"type":"object"}""", PrimitiveSchemaDefinition.decoder),
      """$.type: expected one of "string", "number", "integer", "boolean", "array", found "object""""
    )
    assertEquals(rendered("""{}""", PrimitiveSchemaDefinition.decoder), "$.type: missing required field")

  test("discrimination: a member that matches nothing is a path-tagged error, not a silent string"):
    // An array schema with no items matches neither multi-select; the complaint says which field is
    // missing rather than quietly reading it as something else.
    assertEquals(
      rendered("""{"type":"array"}""", PrimitiveSchemaDefinition.decoder),
      "$.items: missing required field"
    )
    assertEquals(
      rendered("""{"type":"array","items":{}}""", PrimitiveSchemaDefinition.decoder),
      "$.items.type: missing required field"
    )
    assertEquals(
      rendered("""{"type":"array","items":{"type":"string"}}""", PrimitiveSchemaDefinition.decoder),
      "$.items.enum: missing required field"
    )

  test("discrimination: every member survives the union it was chosen out of"):
    for schema <- Sample.primitiveSchemas do
      assertRoundTrips(schema, PrimitiveSchemaDefinition.decoder, PrimitiveSchemaDefinition.encoder)

  // --- the requested schema ------------------------------------------------------------------

  test("requested schema: properties keep the order the server declared them in"):
    assertGolden(Golden.requestedSchema, Sample.requestedSchema, RequestedSchema.decoder, RequestedSchema.encoder)

  test("requested schema: properties is required, and type must be object"):
    assertEquals(
      rendered("""{"type":"object"}""", RequestedSchema.decoder),
      "$.properties: missing required field"
    )
    assertEquals(
      rendered("""{"type":"string","properties":{}}""", RequestedSchema.decoder),
      """$.type: expected "object", found "string""""
    )

  test("requested schema: a broken property is located by its name"):
    assertEquals(
      rendered("""{"type":"object","properties":{"a":{"type":"string"},"b":{"type":"nope"}}}""", RequestedSchema.decoder),
      """$.properties.b.type: expected one of "string", "number", "integer", "boolean", "array", found "nope""""
    )

  // --- the request ---------------------------------------------------------------------------

  test("elicit params: the form mode"):
    assertGolden(Golden.elicitForm, Sample.elicitForm, ElicitRequestParams.decoder, ElicitRequestParams.encoder)

  test("elicit params: an absent mode is the form mode"):
    val withoutMode = s"""{"message":"Who are you?","requestedSchema":${Golden.requestedSchema}}"""
    assertEquals(decoded(withoutMode, ElicitRequestParams.decoder), Sample.elicitForm)

  test("elicit params: the mode is always written back, so the reader need not know the default"):
    val withoutMode = s"""{"message":"Who are you?","requestedSchema":${Golden.requestedSchema}}"""
    assertEquals(
      Codec.encode(decoded(withoutMode, ElicitRequestParams.decoder), ElicitRequestParams.encoder),
      Golden.elicitForm
    )

  test("elicit params: the url mode"):
    assertGolden(Golden.elicitUrl, Sample.elicitUrl, ElicitRequestParams.decoder, ElicitRequestParams.encoder)
    assertGolden(Golden.elicitUrlFull, Sample.elicitUrlFull, ElicitRequestParams.decoder, ElicitRequestParams.encoder)

  test("elicit params: url mode needs its id and its url"):
    assertEquals(
      rendered("""{"mode":"url","message":"m","url":"https://x.test"}""", ElicitRequestParams.decoder),
      "$.elicitationId: missing required field"
    )
    assertEquals(
      rendered("""{"mode":"url","message":"m","elicitationId":"e"}""", ElicitRequestParams.decoder),
      "$.url: missing required field"
    )

  test("elicit params: any other mode is refused, naming both real ones"):
    assertEquals(
      rendered("""{"mode":"modal","message":"m"}""", ElicitRequestParams.decoder),
      """$.mode: expected one of "form", "url", found "modal""""
    )

  test("elicit params: message is required in both modes"):
    assertEquals(
      rendered("""{"requestedSchema":{"type":"object","properties":{}}}""", ElicitRequestParams.decoder),
      "$.message: missing required field"
    )
    assertEquals(
      rendered("""{"mode":"url","elicitationId":"e","url":"u"}""", ElicitRequestParams.decoder),
      "$.message: missing required field"
    )

  // --- the result and the notification -------------------------------------------------------

  test("elicit result: the three actions, with content only where it makes sense"):
    assertEquals(ElicitAction.values.map(_.wire).toVector, Vector("accept", "decline", "cancel"))
    assertGolden(Golden.elicitResultMinimal, Sample.elicitResultMinimal, ElicitResult.decoder, ElicitResult.encoder)
    assertGolden(Golden.elicitResultFull, Sample.elicitResultFull, ElicitResult.decoder, ElicitResult.encoder)

  test("elicit result: the action is required and closed"):
    assertEquals(rendered("{}", ElicitResult.decoder), "$.action: missing required field")
    assertEquals(
      rendered("""{"action":"ok"}""", ElicitResult.decoder),
      """$.action: expected one of "accept", "decline", "cancel", found "ok""""
    )

  test("elicit result: content is carried verbatim, values of every allowed shape"):
    val text = """{"action":"accept","content":{"s":"a","n":1,"b":true,"m":["x","y"]}}"""
    assertEquals(Codec.encode(decoded(text, ElicitResult.decoder), ElicitResult.encoder), text)

  test("elicitation complete: an id, and per the schema no _meta at all"):
    assertGolden(
      Golden.elicitationComplete,
      ElicitationCompleteNotificationParams("e-1"),
      ElicitationCompleteNotificationParams.decoder,
      ElicitationCompleteNotificationParams.encoder
    )
    assertEquals(
      rendered("{}", ElicitationCompleteNotificationParams.decoder),
      "$.elicitationId: missing required field"
    )

  // --- the -32042 error ----------------------------------------------------------------------

  test("url elicitation required: the data hangs off an ordinary error"):
    val data = UrlElicitationRequiredData(Vector(Sample.elicitUrl))
    val error = UrlElicitationRequiredData.toError("Authorization required", data)
    assertEquals(error.code, ErrorCode.UrlElicitationRequired)
    assertEquals(error.code, -32042.0)
    assertEquals(
      Codec.encode(error, JsonRpc.Error.encoder),
      s"""{"code":-32042,"message":"Authorization required","data":{"elicitations":[${Golden.elicitUrl}]}}"""
    )
    assertEquals(UrlElicitationRequiredData.fromError(Path.root / "error", error), Right(data))

  test("url elicitation required: another code is refused, and the numbers read as numbers"):
    assertEquals(
      UrlElicitationRequiredData
        .fromError(Path.root / "error", JsonRpc.Error(ErrorCode.InternalError, "boom", None))
        .left
        .map(_.render),
      Left("$.error.code: expected -32042, found -32603")
    )

  test("url elicitation required: an error with no data is a missing field"):
    assertEquals(
      UrlElicitationRequiredData
        .fromError(Path.root / "error", JsonRpc.Error(ErrorCode.UrlElicitationRequired, "x", None))
        .left
        .map(_.render),
      Left("$.error.data: missing required field")
    )

  test("url elicitation required: a broken elicitation inside is located"):
    val error = JsonRpc.Error(
      ErrorCode.UrlElicitationRequired,
      "x",
      Some(Json.obj("elicitations" -> Json.arr(Json.obj("mode" -> Json.Str("form")))))
    )
    assertEquals(
      UrlElicitationRequiredData.fromError(Path.root / "error", error).left.map(_.render),
      Left("""$.error.data.elicitations[0].mode: expected "url", found "form"""")
    )
