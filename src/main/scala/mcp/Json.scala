package mcp

import scala.scalajs.js

/** An immutable JSON value, for the places the protocol declines to say what it holds.
  *
  * This is deliberately *not* the representation the message decoders walk. Those read the parsed
  * `js.Any` straight off `JSON.parse`, because building a tree only to take it apart again pays for
  * every field twice. `Json` exists for the positions where MCP hands over an unspecified blob and
  * we have to carry it verbatim: `_meta`, tool `arguments`, `inputSchema`/`outputSchema`
  * `properties`, `structuredContent`, sampling `metadata`, `experimental` capabilities, error
  * `data`, elicit `content`. Those we cannot type, so we keep them, and keeping them means owning a
  * value with structural equality that survives a round trip.
  *
  * [[Obj]] preserves the field order it was built with, which is what makes byte-exact encoding
  * tests possible. Two caveats, both inherited from JavaScript rather than chosen here:
  *
  *   - Keys that look like array indices (`"0"`, `"7"`) are hoisted to the front, in ascending
  *     numeric order, by every JS object — so `fromJs(toJs(x)) == x` can reorder such an [[Obj]].
  *     Real `_meta` keys are reverse-DNS-ish and never trip this.
  *   - Duplicate keys cannot reach us from `JSON.parse` (V8 keeps the last one), so [[Obj]] does not
  *     deduplicate on construction. Building one by hand with a repeated key and sending it through
  *     [[toJs]] collapses it the same way V8 would.
  */
enum Json:
  case Null
  case Bool(value: Boolean)
  case Num(value: Double)
  case Str(value: String)
  case Arr(elements: Vector[Json])
  case Obj(fields: Vector[(String, Json)])

object Json:

  /** `Json.obj("a" -> Json.Num(1))`, for hand-built values and test vectors. */
  def obj(fields: (String, Json)*): Json.Obj = Json.Obj(fields.toVector)

  /** `Json.arr(Json.Str("a"))`, for hand-built values and test vectors. */
  def arr(elements: Json*): Json.Arr = Json.Arr(elements.toVector)

  extension (self: Json.Obj)
    /** The first field named `key`, or `None`. First rather than last because [[Obj]] does not
      * deduplicate; anything that came off the wire has one entry per key anyway.
      */
    def get(key: String): Option[Json] =
      self.fields.collectFirst { case (name, value) if name == key => value }

  /** Reads a parsed JavaScript value into the ADT.
    *
    * Everything JSON cannot express — `undefined`, functions, symbols — becomes [[Null]]. That case
    * is unreachable from `JSON.parse` and only exists so this is total.
    */
  def fromJs(value: js.Any): Json =
    if value == null then Json.Null
    else
      js.typeOf(value) match
        case "boolean" => Json.Bool(value.asInstanceOf[Boolean])
        case "number"  => Json.Num(value.asInstanceOf[Double])
        case "string"  => Json.Str(value.asInstanceOf[String])
        case "object" =>
          if js.Array.isArray(value) then
            Json.Arr(value.asInstanceOf[js.Array[js.Any]].toVector.map(fromJs))
          else
            val record = value.asInstanceOf[js.Dictionary[js.Any]]
            val keys = js.Object.keys(value.asInstanceOf[js.Object])
            Json.Obj(keys.toVector.map(key => key -> fromJs(record(key))))
        case _ => Json.Null

  /** [[fromJs]] where the value is known to be an object, which every encoder in this package that
    * writes a result or a params block produces. A non-object reads as the empty object rather than
    * throwing; nothing in this package can reach that branch.
    */
  private[mcp] def objFromJs(value: js.Any): Json.Obj =
    fromJs(value) match
      case fields: Json.Obj => fields
      case _                => Json.Obj(Vector.empty)

  /** Builds the JavaScript value `JSON.stringify` will render. */
  def toJs(value: Json): js.Any =
    value match
      case Json.Null        => null
      case Json.Bool(flag)  => flag
      case Json.Num(number) => number
      case Json.Str(text)   => text
      case Json.Arr(items)  => js.Array(items.map(toJs)*)
      case Json.Obj(fields) =>
        val record = js.Dynamic.literal()
        fields.foreach((key, field) => record.updateDynamic(key)(toJs(field)))
        record

  /** `JSON.parse`, with its `SyntaxError` turned into a `Left`.
    *
    * The `Left` carries V8's own complaint, which is useful in a log and useless as a contract: its
    * wording moves between Node versions. [[DecodeError.Malformed]] wraps it for exactly that
    * reason — the rendered error says only that the text was not JSON.
    */
  def parse(text: String): Either[String, Json] =
    try Right(fromJs(js.JSON.parse(text)))
    catch case js.JavaScriptException(error) => Left(error.toString)

  def stringify(value: Json): String = js.JSON.stringify(toJs(value))
