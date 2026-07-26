package mcp

import scala.scalajs.js

/** Where in a message a decoder was standing when it gave up.
  *
  * Rendered the way a JavaScript programmer would write the access: `$` for the whole message, then
  * `.field` and `[index]`. The rendering is a contract — see [[DecodeError.render]].
  */
final case class Path(segments: Vector[Path.Segment]):
  def /(name: String): Path = Path(segments :+ Path.Segment.Field(name))
  def apply(index: Int): Path = Path(segments :+ Path.Segment.Index(index))

  def render: String =
    val out = StringBuilder("$")
    segments.foreach:
      case Path.Segment.Field(name)  => out.append('.').append(name)
      case Path.Segment.Index(index) => out.append('[').append(index).append(']')
    out.result()

object Path:
  val root: Path = Path(Vector.empty)

  enum Segment:
    case Field(name: String)
    case Index(index: Int)

/** Why a decoder refused the input.
  *
  * [[render]] is the contract. Every case produces one line, `path: complaint`, and the tests pin
  * the exact strings — an error message a caller prints is as much a public surface as a return
  * type. Decoding is fail-fast: the first refusal is the answer, so a message never accumulates a
  * list of complaints.
  *
  * [[UnknownMethod]] is separate from a plain [[Mismatch]] on purpose. A dispatcher owes the sender
  * `-32601 Method not found` for exactly this case and nothing else, and it should not have to
  * recognise that situation by matching on English.
  */
enum DecodeError:
  /** The value was there and was the wrong thing. `found` names the JavaScript runtime kind — null,
    * boolean, number, string, array, object, undefined — except where the complaint is about a
    * value rather than a kind, in which case it is that value, quoted.
    */
  case Mismatch(path: Path, expected: String, found: String)

  /** A required field was absent. `path` names the missing field, not the object that lacks it. */
  case Missing(path: Path)

  /** The `method` string named no message type this package models. */
  case UnknownMethod(path: Path, method: String)

  /** `JSON.parse` rejected the text. `detail` is V8's own wording, kept for logs and deliberately
    * left out of [[render]]: it is not stable across Node versions, and a contract that moves is
    * not a contract.
    */
  case Malformed(detail: String)

  def render: String =
    this match
      case Mismatch(path, expected, found) => s"${path.render}: expected $expected, found $found"
      case Missing(path)                   => s"${path.render}: missing required field"
      case UnknownMethod(path, method) => s"${path.render}: unknown method ${Codec.quote(method)}"
      case Malformed(_)                => s"${Path.root.render}: not valid JSON"

/** A member of a closed string union, carrying the spelling it has on the wire.
  *
  * MCP's closed unions — [[Role]], [[LoggingLevel]], [[IncludeContext]] and the rest — are Scala
  * enums here rather than bare strings, so that an unrecognised value is caught at the boundary
  * instead of somewhere downstream. Keeping the wire spelling on the case means the enum's
  * declaration order, the decoder's list of alternatives and the encoder's output all come from one
  * place and cannot drift apart.
  */
trait Wire:
  def wire: String

/** Reads a value of type `A` out of a parsed JavaScript value. */
trait Decoder[A]:
  def decode(at: Path, value: js.Any): Either[DecodeError, A]

  final def map[B](f: A => B): Decoder[B] =
    (at, value) => decode(at, value).map(f)

  /** For the checks a shape alone cannot express — a literal `"object"` in a JSON Schema, a union
    * member picked out by the fields around it.
    */
  final def emap[B](f: (Path, A) => Either[DecodeError, B]): Decoder[B] =
    (at, value) => decode(at, value).flatMap(f(at, _))

/** Builds the JavaScript value for an `A`. Total: encoding cannot fail. */
trait Encoder[A]:
  def encode(value: A): js.Any

/** The fields of an object that a decoder has already established is an object.
  *
  * Lookups go through the raw property, which is safe here only because no MCP field name collides
  * with anything on `Object.prototype`; a decoder that needed `constructor` or `toString` would
  * have to ask `hasOwnProperty` first.
  */
final class Fields private[mcp] (val path: Path, private val record: js.Dynamic):

  /** The property as it came off the wire — `undefined` when absent. For discriminating unions. */
  def raw(name: String): js.Any = record.selectDynamic(name)

  /** Whether the field carries a value. Explicit `null` counts as absent, matching the leniency
    * [[opt]] applies: a sender that spells "nothing" as `null` means the same thing as one that
    * leaves the key out.
    */
  def present(name: String): Boolean =
    val value = raw(name)
    value != null && !js.isUndefined(value)

  def req[A](name: String, decoder: Decoder[A]): Either[DecodeError, A] =
    val value = raw(name)
    if js.isUndefined(value) then Left(DecodeError.Missing(path / name))
    else decoder.decode(path / name, value)

  def opt[A](name: String, decoder: Decoder[A]): Either[DecodeError, Option[A]] =
    if !present(name) then Right(None)
    else decoder.decode(path / name, raw(name)).map(Some(_))

/** An object under construction, written in field order.
  *
  * Insertion order is the encoding contract: JavaScript objects iterate string keys in the order
  * they were added, `JSON.stringify` follows that iteration, and the golden tests pin the bytes.
  * Absent optional fields are omitted rather than written as `null` — see [[Encode]].
  */
final class JsObj private[mcp] ():
  private val record: js.Dynamic = js.Dynamic.literal()

  def set(name: String, value: js.Any): JsObj =
    record.updateDynamic(name)(value)
    this

  def put[A](name: String, value: A, encoder: Encoder[A]): JsObj =
    set(name, encoder.encode(value))

  def putOpt[A](name: String, value: Option[A], encoder: Encoder[A]): JsObj =
    value match
      case Some(present) => set(name, encoder.encode(present))
      case None          => this

  /** An optional arbitrary-JSON field, where `Some(Json.Null)` is an absence.
    *
    * The only field in the package whose optional value can itself *be* `null` is an error's
    * `data`. Writing that out would produce a member that reads back as `None` — the leniency rule
    * turns a `null` optional into an absence — so it is omitted instead, and the rule that encoders
    * never emit `null` holds without exception.
    */
  def putJson(name: String, value: Option[Json]): JsObj =
    value match
      case None | Some(Json.Null) => this
      case Some(present)          => set(name, Encode.json.encode(present))

  def result: js.Any = record

/** Decoder combinators. Hand-written instances compose out of these; there are no macros here.
  *
  * Two conventions run through all of them, both from the spec's JSON rules:
  *   - an unknown field is ignored, so a peer may add fields without breaking us;
  *   - an optional field spelled `null` reads as absent, while a required field spelled `null` is an
  *     error that names `null` as what it found.
  *
  * The scalar decoders need no separate null check: `typeof null` is `"object"` in JavaScript, so a
  * `null` falls through to the mismatch, and [[Codec.kindOf]] is the one place that tells the two
  * apart.
  */
object Decode:

  val string: Decoder[String] = (at, value) =>
    if js.typeOf(value) == "string" then Right(value.asInstanceOf[String])
    else Left(DecodeError.Mismatch(at, "a string", Codec.kindOf(value)))

  val number: Decoder[Double] = (at, value) =>
    if js.typeOf(value) == "number" then Right(value.asInstanceOf[Double])
    else Left(DecodeError.Mismatch(at, "a number", Codec.kindOf(value)))

  val boolean: Decoder[Boolean] = (at, value) =>
    if js.typeOf(value) == "boolean" then Right(value.asInstanceOf[Boolean])
    else Left(DecodeError.Mismatch(at, "a boolean", Codec.kindOf(value)))

  /** Anything at all, carried verbatim. `null` is a value here, not an absence. */
  val json: Decoder[Json] = (_, value) => Right(Json.fromJs(value))

  val jsonObj: Decoder[Json.Obj] = (at, value) =>
    Json.fromJs(value) match
      case fields: Json.Obj => Right(fields)
      case _                => Left(DecodeError.Mismatch(at, "an object", Codec.kindOf(value)))

  def vector[A](element: Decoder[A]): Decoder[Vector[A]] = (at, value) =>
    if !js.Array.isArray(value) then Left(DecodeError.Mismatch(at, "an array", Codec.kindOf(value)))
    else
      val items = value.asInstanceOf[js.Array[js.Any]]
      val decoded = Vector.newBuilder[A]
      var failure: Option[DecodeError] = None
      var index = 0
      while index < items.length && failure.isEmpty do
        element.decode(at(index), items(index)) match
          case Right(item)  => decoded += item
          case Left(reason) => failure = Some(reason)
        index += 1
      failure.toLeft(decoded.result())

  def obj[A](read: Fields => Either[DecodeError, A]): Decoder[A] = (at, value) =>
    if Codec.kindOf(value) != "object" then
      Left(DecodeError.Mismatch(at, "an object", Codec.kindOf(value)))
    else read(new Fields(at, value.asInstanceOf[js.Dynamic]))

  /** A closed string union. An unrecognised value is an error that lists the alternatives and
    * quotes the offender, rather than a silent fallback to some default member.
    */
  def stringEnum[A](members: (String, A)*): Decoder[A] =
    val expected = members.map((name, _) => Codec.quote(name)).mkString("one of ", ", ", "")
    (at, value) =>
      string.decode(at, value).flatMap: text =>
        members.collectFirst { case (name, member) if name == text => member } match
          case Some(member) => Right(member)
          case None         => Left(DecodeError.Mismatch(at, expected, Codec.quote(text)))

  /** [[stringEnum]] for an enum whose cases already know their wire spelling. The alternatives are
    * listed in declaration order, which mirrors the schema's.
    */
  def wireEnum[A <: Wire](members: Array[A]): Decoder[A] =
    stringEnum(members.toIndexedSeq.map(member => member.wire -> member)*)

  /** A string that must be one specific value — the `type: "object"` of a JSON Schema, the `"2.0"`
    * of a JSON-RPC envelope.
    */
  def literal(text: String): Decoder[String] =
    string.emap: (at, found) =>
      if found == text then Right(found)
      else Left(DecodeError.Mismatch(at, Codec.quote(text), Codec.quote(found)))

/** Encoder combinators. */
object Encode:

  val string: Encoder[String] = value => value
  val number: Encoder[Double] = value => value
  val boolean: Encoder[Boolean] = value => value
  val json: Encoder[Json] = Json.toJs(_)
  val jsonObj: Encoder[Json.Obj] = Json.toJs(_)

  def wire[A <: Wire]: Encoder[A] = value => string.encode(value.wire)

  def vector[A](element: Encoder[A]): Encoder[Vector[A]] =
    values => js.Array(values.map(element.encode)*)

  def obj[A](write: (A, JsObj) => Unit): Encoder[A] =
    value =>
      val out = new JsObj()
      write(value, out)
      out.result

/** The boundary: text in, typed value out, and back again.
  *
  * One `JSON.parse`, one walk, one `JSON.stringify`. Nothing compiled from Scala competes with
  * V8's parser, so the only tree we ever build is the one the caller asked for.
  */
object Codec:

  /** The JavaScript runtime kind of a value, in the words [[DecodeError.Mismatch]] uses. */
  def kindOf(value: js.Any): String =
    if value == null then "null"
    else
      js.typeOf(value) match
        case "object" => if js.Array.isArray(value) then "array" else "object"
        case other    => other

  /** A JSON string literal, escapes and all — the errors quote user input, which may contain
    * anything.
    */
  def quote(text: String): String = js.JSON.stringify(text)

  /** A number as JavaScript writes it, for error messages. Scala's own `Double.toString` would say
    * `-32603.0`, which is not a number anyone sent.
    */
  def show(number: Double): String = js.JSON.stringify(number)

  def parse(text: String): Either[DecodeError, js.Any] =
    try Right(js.JSON.parse(text))
    catch case js.JavaScriptException(error) => Left(DecodeError.Malformed(error.toString))

  def decode[A](text: String, decoder: Decoder[A]): Either[DecodeError, A] =
    parse(text).flatMap(decoder.decode(Path.root, _))

  def encode[A](value: A, encoder: Encoder[A]): String =
    js.JSON.stringify(encoder.encode(value))

  /** A message a client sends: a request, a notification, or its answer to something the server
    * asked.
    */
  def decodeClientMessage(text: String): Either[DecodeError, ClientMessage] =
    decode(text, ClientMessage.decoder)

  /** A message a server sends. */
  def decodeServerMessage(text: String): Either[DecodeError, ServerMessage] =
    decode(text, ServerMessage.decoder)

  def encodeClientMessage(message: ClientMessage): String =
    encode(message, ClientMessage.encoder)

  def encodeServerMessage(message: ServerMessage): String =
    encode(message, ServerMessage.encoder)
