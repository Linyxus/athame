package mcp

/** The codec kernel: paths, error rendering, and the combinators every family is built from.
  *
  * [[DecodeError.render]] is a contract, so the strings here are pinned verbatim. They are what a
  * caller logs and what a peer is told, and a change to any of them is a change to a public
  * surface — which is exactly what a failing assertion should say.
  */
class CodecSuite extends McpSuite:

  // --- paths ---------------------------------------------------------------------------------

  test("path: renders as a JavaScript access, root included"):
    assertEquals(Path.root.render, "$")
    assertEquals((Path.root / "params").render, "$.params")
    assertEquals((Path.root / "params" / "name").render, "$.params.name")
    assertEquals(Path.root(0).render, "$[0]")
    assertEquals((Path.root / "params" / "tools")(2).render, "$.params.tools[2]")
    assertEquals(((Path.root / "params" / "tools")(2) / "name").render, "$.params.tools[2].name")

  test("path: building one leaves the original alone"):
    val base = Path.root / "params"
    assertEquals((base / "a").render, "$.params.a")
    assertEquals((base / "b").render, "$.params.b")
    assertEquals(base.render, "$.params")

  // --- error rendering -----------------------------------------------------------------------

  test("errors: each case renders one line of path and complaint"):
    assertEquals(
      DecodeError.Mismatch((Path.root / "params" / "tools")(2) / "name", "a string", "number").render,
      "$.params.tools[2].name: expected a string, found number"
    )
    assertEquals(
      DecodeError.Missing(Path.root / "params" / "name").render,
      "$.params.name: missing required field"
    )
    assertEquals(
      DecodeError.UnknownMethod(Path.root / "method", "tools/frobnicate").render,
      """$.method: unknown method "tools/frobnicate""""
    )

  test("errors: a malformed document says only that, whatever V8 called it"):
    // V8's wording moves between Node releases; pinning it would make this suite a Node version
    // test. The detail is still carried, for logs.
    assertEquals(DecodeError.Malformed("SyntaxError: whatever").render, "$: not valid JSON")
    assertEquals(DecodeError.Malformed("").render, "$: not valid JSON")

  test("errors: quoting an offender escapes it, because offenders are user input"):
    assertEquals(Codec.quote("plain"), "\"plain\"")
    assertEquals(Codec.quote("say \"hi\""), """"say \"hi\""""")
    assertEquals(Codec.quote("a\nb"), """"a\nb"""")

  test("errors: a number in a message reads as JavaScript writes it"):
    // Scala's Double.toString would say -32603.0, which is not a number anybody sent.
    assertEquals(Codec.show(-32603), "-32603")
    assertEquals(Codec.show(1.5), "1.5")

  // --- runtime kinds -------------------------------------------------------------------------

  test("kinds: found names what JavaScript would call it"):
    assertEquals(Codec.kindOf(null), "null")
    assertEquals(Codec.kindOf(true), "boolean")
    assertEquals(Codec.kindOf(1), "number")
    assertEquals(Codec.kindOf("x"), "string")
    assertEquals(Codec.kindOf(scala.scalajs.js.Array[scala.scalajs.js.Any](1)), "array")
    assertEquals(Codec.kindOf(scala.scalajs.js.Dynamic.literal()), "object")
    assertEquals(Codec.kindOf(scala.scalajs.js.undefined), "undefined")

  // --- scalar decoders -----------------------------------------------------------------------

  test("decode: a scalar of the wrong kind names both sides"):
    assertEquals(rendered("1", Decode.string), "$: expected a string, found number")
    assertEquals(rendered("\"x\"", Decode.number), "$: expected a number, found string")
    assertEquals(rendered("[]", Decode.boolean), "$: expected a boolean, found array")
    assertEquals(rendered("{}", Decode.string), "$: expected a string, found object")
    assertEquals(rendered("null", Decode.string), "$: expected a string, found null")

  test("decode: scalars of the right kind come through unchanged"):
    assertEquals(decoded("\"x\"", Decode.string), "x")
    assertEquals(decoded("1.5", Decode.number), 1.5)
    assertEquals(decoded("-0", Decode.number), -0.0)
    assertEquals(decoded("true", Decode.boolean), true)

  // --- objects and fields --------------------------------------------------------------------

  private val pair: Decoder[(String, Option[Double])] = Decode.obj: fields =>
    for
      name <- fields.req("name", Decode.string)
      size <- fields.opt("size", Decode.number)
    yield (name, size)

  test("decode: a missing required field is reported at the field, not at its parent"):
    assertEquals(rendered("""{}""", pair), "$.name: missing required field")
    assertEquals(rendered("""{"size":1}""", pair), "$.name: missing required field")

  test("decode: null where a required field belongs names null as what it found"):
    assertEquals(rendered("""{"name":null}""", pair), "$.name: expected a string, found null")

  test("decode: null where an optional field belongs is an absence"):
    // The leniency rule: a peer that spells "nothing" as null means what a peer that omits the key
    // means.
    assertEquals(decoded("""{"name":"a","size":null}""", pair), ("a", None))
    assertEquals(decoded("""{"name":"a"}""", pair), ("a", None))
    assertEquals(decoded("""{"name":"a","size":2}""", pair), ("a", Some(2.0)))

  test("decode: unknown fields are ignored, so a peer may grow new ones"):
    assertEquals(decoded("""{"name":"a","future":{"deep":[1]},"size":2}""", pair), ("a", Some(2.0)))

  test("decode: something that is not an object says so"):
    assertEquals(rendered("[]", pair), "$: expected an object, found array")
    assertEquals(rendered("null", pair), "$: expected an object, found null")
    assertEquals(rendered("\"x\"", pair), "$: expected an object, found string")

  // --- vectors -------------------------------------------------------------------------------

  test("decode: an array element's complaint carries its index"):
    val strings = Decode.vector(Decode.string)
    assertEquals(decoded("""["a","b"]""", strings), Vector("a", "b"))
    assertEquals(decoded("[]", strings), Vector.empty[String])
    assertEquals(rendered("""["a",2]""", strings), "$[1]: expected a string, found number")
    assertEquals(rendered("""["a","b",null]""", strings), "$[2]: expected a string, found null")

  test("decode: a non-array where an array belongs says so"):
    assertEquals(rendered("{}", Decode.vector(Decode.string)), "$: expected an array, found object")

  test("decode: the first failure is the answer, and the rest is not looked at"):
    // Fail-fast: two broken elements produce the first one's complaint, not a list.
    assertEquals(rendered("""[1,2]""", Decode.vector(Decode.string)), "$[0]: expected a string, found number")

  // --- closed unions and literals ------------------------------------------------------------

  test("decode: a closed union lists its alternatives and quotes the offender"):
    assertEquals(
      rendered("\"verbose\"", LoggingLevel.decoder),
      """$: expected one of "debug", "info", "notice", "warning", "error", "critical", "alert", "emergency", found "verbose""""
    )
    assertEquals(rendered("\"USER\"", Role.decoder), """$: expected one of "user", "assistant", found "USER"""")

  test("decode: a closed union given the wrong kind complains about the kind"):
    assertEquals(rendered("1", Role.decoder), "$: expected a string, found number")

  test("decode: a literal names the value it wanted"):
    assertEquals(decoded("\"2.0\"", Decode.literal("2.0")), "2.0")
    assertEquals(rendered("\"1.0\"", Decode.literal("2.0")), """$: expected "2.0", found "1.0"""")

  // --- json positions ------------------------------------------------------------------------

  test("decode: an arbitrary-JSON position takes anything, null included"):
    assertEquals(decoded("null", Decode.json), Json.Null)
    assertEquals(decoded("[1]", Decode.json), Json.arr(Json.Num(1)))
    assertEquals(decoded("""{"a":1}""", Decode.json), Json.obj("a" -> Json.Num(1)))

  test("decode: an object-only JSON position refuses everything else"):
    assertEquals(decoded("""{"a":1}""", Decode.jsonObj), Json.obj("a" -> Json.Num(1)))
    assertEquals(rendered("[1]", Decode.jsonObj), "$: expected an object, found array")
    assertEquals(rendered("null", Decode.jsonObj), "$: expected an object, found null")

  // --- the text boundary ---------------------------------------------------------------------

  test("codec: malformed text never escapes as an exception"):
    assertEquals(Codec.decode("{", Decode.jsonObj).map(_ => ()).left.map(_.render), Left("$: not valid JSON"))
    assertEquals(Codec.decode("", Decode.jsonObj).left.map(_.render), Left("$: not valid JSON"))
    assertEquals(Codec.decode("undefined", Decode.jsonObj).left.map(_.render), Left("$: not valid JSON"))

  test("codec: encoding stringifies once, with no spaces of its own"):
    assertEquals(Codec.encode(Json.obj("a" -> Json.Num(1)), Encode.jsonObj), """{"a":1}""")

  // --- encoders ------------------------------------------------------------------------------

  test("encode: an absent optional is omitted, never written as null"):
    // Every optional field in the package goes through putOpt, so this is the whole rule.
    assertEquals(Codec.encode(Annotations(None, None, None), Annotations.encoder), "{}")
    assertEquals(
      Codec.encode(Annotations(None, Some(0.5), None), Annotations.encoder),
      """{"priority":0.5}"""
    )

  test("encode: field order is declaration order, every time"):
    assertEquals(
      Codec.encode(Annotations(Some(Vector(Role.User)), Some(1), Some("t")), Annotations.encoder),
      """{"audience":["user"],"priority":1,"lastModified":"t"}"""
    )

  test("encode: an integral double carries no decimal point into the output"):
    assertEquals(
      Codec.encode(RequestId.Num(1), RequestId.encoder),
      "1"
    )
    assertEquals(Codec.encode(ProgressToken.Num(-7), ProgressToken.encoder), "-7")

  // --- request ids and progress tokens -------------------------------------------------------

  test("ids: both spellings decode, and each keeps the spelling it arrived in"):
    assertEquals(decoded("\"abc\"", RequestId.decoder), RequestId.Str("abc"))
    assertEquals(decoded("42", RequestId.decoder), RequestId.Num(42))
    assertEquals(Codec.encode(RequestId.Str("abc"), RequestId.encoder), "\"abc\"")
    assertEquals(Codec.encode(RequestId.Num(42), RequestId.encoder), "42")

  test("ids: anything else is refused, naming both alternatives"):
    assertEquals(rendered("null", RequestId.decoder), "$: expected a string or number, found null")
    assertEquals(rendered("true", RequestId.decoder), "$: expected a string or number, found boolean")
    assertEquals(rendered("[]", RequestId.decoder), "$: expected a string or number, found array")
    assertEquals(rendered("{}", ProgressToken.decoder), "$: expected a string or number, found object")

  // --- the progress token helpers ------------------------------------------------------------

  test("meta: a progress token is read out of _meta, when there is one to read"):
    assertEquals(ProgressToken.from(None), None)
    assertEquals(ProgressToken.from(Some(Json.obj())), None)
    assertEquals(ProgressToken.from(Some(Json.obj("progressToken" -> Json.Str("p")))), Some(ProgressToken.Str("p")))
    assertEquals(ProgressToken.from(Some(Json.obj("progressToken" -> Json.Num(4)))), Some(ProgressToken.Num(4)))

  test("meta: a token of the wrong shape is no token"):
    assertEquals(ProgressToken.from(Some(Json.obj("progressToken" -> Json.Null))), None)
    assertEquals(ProgressToken.from(Some(Json.obj("progressToken" -> Json.arr()))), None)

  test("meta: writing a token keeps the rest of _meta, contents and order"):
    val existing = Json.obj("a" -> Json.Num(1), "b" -> Json.Num(2))
    assertEquals(
      ProgressToken.into(Some(existing), ProgressToken.Str("p")),
      Json.obj("a" -> Json.Num(1), "b" -> Json.Num(2), "progressToken" -> Json.Str("p"))
    )
    assertEquals(ProgressToken.into(None, ProgressToken.Num(1)), Json.obj("progressToken" -> Json.Num(1)))

  test("meta: rewriting a token replaces it in place rather than appending a second"):
    val existing = Json.obj("progressToken" -> Json.Str("old"), "z" -> Json.Num(1))
    assertEquals(
      ProgressToken.into(Some(existing), ProgressToken.Str("new")),
      Json.obj("progressToken" -> Json.Str("new"), "z" -> Json.Num(1))
    )

  test("meta: reading back what was written is the identity on tokens"):
    for token <- Vector(ProgressToken.Str("p"), ProgressToken.Num(0), ProgressToken.Num(-1.5)) do
      assertEquals(ProgressToken.from(Some(ProgressToken.into(None, token))), Some(token))
