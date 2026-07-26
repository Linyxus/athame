package mcp

import scala.scalajs.js

/** `elicitation/create`: a server asking the user a question through the client
  * (schema.ts:2150-2492).
  *
  * Two shapes of question. A form describes the answer it wants with a restricted JSON Schema and
  * gets the values back inline; a URL sends the user somewhere and gets a notification when they
  * are done. They are one union on the wire, discriminated by `mode`.
  */

/** The elicitation actions a user can take (schema.ts:2462-2469). */
enum ElicitAction(val wire: String) extends Wire:
  case Accept extends ElicitAction("accept")
  case Decline extends ElicitAction("decline")
  case Cancel extends ElicitAction("cancel")

object ElicitAction:
  val decoder: Decoder[ElicitAction] = Decode.wireEnum(ElicitAction.values)
  val encoder: Encoder[ElicitAction] = Encode.wire

/** The formats a string field may declare (schema.ts:2246). */
enum StringFormat(val wire: String) extends Wire:
  case Email extends StringFormat("email")
  case Uri extends StringFormat("uri")
  case Date extends StringFormat("date")
  case DateTime extends StringFormat("date-time")

object StringFormat:
  val decoder: Decoder[StringFormat] = Decode.wireEnum(StringFormat.values)
  val encoder: Encoder[StringFormat] = Encode.wire

/** Which of the two numeric `type` spellings a [[NumberSchema]] used (schema.ts:2254).
  *
  * Kept rather than collapsed: `integer` and `number` mean different things to whoever renders the
  * form, and a schema that arrived as one must not leave as the other.
  */
enum NumberType(val wire: String) extends Wire:
  case Number extends NumberType("number")
  case Integer extends NumberType("integer")

object NumberType:
  val decoder: Decoder[NumberType] = Decode.wireEnum(NumberType.values)
  val encoder: Encoder[NumberType] = Encode.wire

/** One choice in a titled enum: the value, and what to show for it (schema.ts:2315-2324). */
final case class EnumOption(const: String, title: String)

object EnumOption:
  val decoder: Decoder[EnumOption] = Decode.obj: fields =>
    for
      const <- fields.req("const", Decode.string)
      title <- fields.req("title", Decode.string)
    yield EnumOption(const, title)

  val encoder: Encoder[EnumOption] = Encode.obj: (option, out) =>
    out.put("const", option.const, Encode.string)
    out.put("title", option.title, Encode.string)

/** One field of an elicitation form (schema.ts:2228-2455).
  *
  * ==How a value is recognised==
  *
  * The schema gives these eight members no discriminator of their own — five of them say
  * `type: "string"` — so the rule is derived from the fields around it. This table is normative
  * (ruling M10):
  *
  * {{{
  * | wire shape                              | case                          |
  * |-----------------------------------------|-------------------------------|
  * | type:"boolean"                          | BooleanSchema                 |
  * | type:"number" or "integer"              | NumberSchema                  |
  * | type:"string" with `oneOf`              | TitledSingleSelectEnumSchema  |
  * | type:"string" with `enum` + `enumNames` | LegacyTitledEnumSchema        |
  * | type:"string" with `enum` only          | UntitledSingleSelectEnumSchema|
  * | type:"string" with neither              | StringSchema                  |
  * | type:"array" with `items.anyOf`         | TitledMultiSelectEnumSchema   |
  * | type:"array" with `items.enum`          | UntitledMultiSelectEnumSchema |
  * | anything else                           | path-tagged decode error      |
  * }}}
  *
  * The rows are tested in that order, so `oneOf` wins over `enum` if a sender supplies both.
  * [[NumberSchema]] remembers which of the two numeric spellings it was given. A field spelled
  * `null` counts as absent throughout, the same leniency [[Fields.opt]] applies. A value matching
  * no row is a path-tagged error rather than a quiet fallback to [[StringSchema]].
  *
  * ==The one place the schema is ambiguous==
  *
  * Because `enumNames` is optional in the schema's `LegacyTitledEnumSchema` (schema.ts:2446),
  * Legacy-without-enumNames is byte-identical to [[UntitledSingleSelectEnumSchema]]
  * (schema.ts:2277-2295 against 2437-2448) and the union overlaps. No decoder can tell the two
  * apart.
  *
  * The Scala model therefore takes the *quotient* of the wire language by the table above:
  * `enumNames` is required here, so the ambiguous value is not constructible and decode ∘ encode is
  * the identity on every branch. The evidence that this loses nothing is in the schema itself — it
  * deprecates `LegacyTitledEnumSchema` in favour of [[TitledSingleSelectEnumSchema]]
  * (schema.ts:2431-2436), so the member's sole reason to exist on the wire is `enumNames`. Every
  * wire value still decodes; the narrowing removes only a Scala value we could never have encoded
  * and read back as itself.
  */
sealed trait PrimitiveSchemaDefinition:
  /** The wire `type` of this schema. Not a discriminator on its own — see the rule above. */
  def schemaType: String

/** schema.ts:2450-2455. */
sealed trait EnumSchema extends PrimitiveSchemaDefinition

/** schema.ts:2331-2336. */
sealed trait SingleSelectEnumSchema extends EnumSchema:
  def schemaType: String = "string"

/** schema.ts:2424-2429. */
sealed trait MultiSelectEnumSchema extends EnumSchema:
  def schemaType: String = "array"

/** schema.ts:2237-2248. */
final case class StringSchema(
  title: Option[String],
  description: Option[String],
  minLength: Option[Double],
  maxLength: Option[Double],
  format: Option[StringFormat],
  default: Option[String]
) extends PrimitiveSchemaDefinition:
  def schemaType: String = "string"

object StringSchema:
  private[mcp] def read(fields: Fields): Either[DecodeError, StringSchema] =
    for
      title <- fields.opt("title", Decode.string)
      description <- fields.opt("description", Decode.string)
      minLength <- fields.opt("minLength", Decode.number)
      maxLength <- fields.opt("maxLength", Decode.number)
      format <- fields.opt("format", StringFormat.decoder)
      default <- fields.opt("default", Decode.string)
    yield StringSchema(title, description, minLength, maxLength, format, default)

  private[mcp] def write(schema: StringSchema, out: JsObj): Unit =
    out.put("type", schema.schemaType, Encode.string)
    out.putOpt("title", schema.title, Encode.string)
    out.putOpt("description", schema.description, Encode.string)
    out.putOpt("minLength", schema.minLength, Encode.number)
    out.putOpt("maxLength", schema.maxLength, Encode.number)
    out.putOpt("format", schema.format, StringFormat.encoder)
    out.putOpt("default", schema.default, Encode.string)

  val decoder: Decoder[StringSchema] = Decode.obj: fields =>
    fields.req("type", Decode.literal("string")).flatMap(_ => read(fields))

  val encoder: Encoder[StringSchema] = Encode.obj(write)

/** schema.ts:2250-2260. */
final case class NumberSchema(
  numberType: NumberType,
  title: Option[String],
  description: Option[String],
  minimum: Option[Double],
  maximum: Option[Double],
  default: Option[Double]
) extends PrimitiveSchemaDefinition:
  def schemaType: String = numberType.wire

object NumberSchema:
  private[mcp] def read(fields: Fields): Either[DecodeError, NumberSchema] =
    for
      numberType <- fields.req("type", NumberType.decoder)
      title <- fields.opt("title", Decode.string)
      description <- fields.opt("description", Decode.string)
      minimum <- fields.opt("minimum", Decode.number)
      maximum <- fields.opt("maximum", Decode.number)
      default <- fields.opt("default", Decode.number)
    yield NumberSchema(numberType, title, description, minimum, maximum, default)

  private[mcp] def write(schema: NumberSchema, out: JsObj): Unit =
    out.put("type", schema.numberType, NumberType.encoder)
    out.putOpt("title", schema.title, Encode.string)
    out.putOpt("description", schema.description, Encode.string)
    out.putOpt("minimum", schema.minimum, Encode.number)
    out.putOpt("maximum", schema.maximum, Encode.number)
    out.putOpt("default", schema.default, Encode.number)

  val decoder: Decoder[NumberSchema] = Decode.obj(read)
  val encoder: Encoder[NumberSchema] = Encode.obj(write)

/** schema.ts:2262-2270. */
final case class BooleanSchema(
  title: Option[String],
  description: Option[String],
  default: Option[Boolean]
) extends PrimitiveSchemaDefinition:
  def schemaType: String = "boolean"

object BooleanSchema:
  private[mcp] def read(fields: Fields): Either[DecodeError, BooleanSchema] =
    for
      title <- fields.opt("title", Decode.string)
      description <- fields.opt("description", Decode.string)
      default <- fields.opt("default", Decode.boolean)
    yield BooleanSchema(title, description, default)

  private[mcp] def write(schema: BooleanSchema, out: JsObj): Unit =
    out.put("type", schema.schemaType, Encode.string)
    out.putOpt("title", schema.title, Encode.string)
    out.putOpt("description", schema.description, Encode.string)
    out.putOpt("default", schema.default, Encode.boolean)

  val decoder: Decoder[BooleanSchema] = Decode.obj: fields =>
    fields.req("type", Decode.literal("boolean")).flatMap(_ => read(fields))

  val encoder: Encoder[BooleanSchema] = Encode.obj(write)

/** Pick one, values only (schema.ts:2272-2295). */
final case class UntitledSingleSelectEnumSchema(
  title: Option[String],
  description: Option[String],
  enumValues: Vector[String],
  default: Option[String]
) extends SingleSelectEnumSchema

object UntitledSingleSelectEnumSchema:
  private[mcp] def read(fields: Fields): Either[DecodeError, UntitledSingleSelectEnumSchema] =
    for
      title <- fields.opt("title", Decode.string)
      description <- fields.opt("description", Decode.string)
      enumValues <- fields.req("enum", Decode.vector(Decode.string))
      default <- fields.opt("default", Decode.string)
    yield UntitledSingleSelectEnumSchema(title, description, enumValues, default)

  private[mcp] def write(schema: UntitledSingleSelectEnumSchema, out: JsObj): Unit =
    out.put("type", schema.schemaType, Encode.string)
    out.putOpt("title", schema.title, Encode.string)
    out.putOpt("description", schema.description, Encode.string)
    out.put("enum", schema.enumValues, Encode.vector(Encode.string))
    out.putOpt("default", schema.default, Encode.string)

  val decoder: Decoder[UntitledSingleSelectEnumSchema] = Decode.obj: fields =>
    fields.req("type", Decode.literal("string")).flatMap(_ => read(fields))

  val encoder: Encoder[UntitledSingleSelectEnumSchema] = Encode.obj(write)

/** Pick one, with a label for each (schema.ts:2297-2329). */
final case class TitledSingleSelectEnumSchema(
  title: Option[String],
  description: Option[String],
  oneOf: Vector[EnumOption],
  default: Option[String]
) extends SingleSelectEnumSchema

object TitledSingleSelectEnumSchema:
  private[mcp] def read(fields: Fields): Either[DecodeError, TitledSingleSelectEnumSchema] =
    for
      title <- fields.opt("title", Decode.string)
      description <- fields.opt("description", Decode.string)
      oneOf <- fields.req("oneOf", Decode.vector(EnumOption.decoder))
      default <- fields.opt("default", Decode.string)
    yield TitledSingleSelectEnumSchema(title, description, oneOf, default)

  private[mcp] def write(schema: TitledSingleSelectEnumSchema, out: JsObj): Unit =
    out.put("type", schema.schemaType, Encode.string)
    out.putOpt("title", schema.title, Encode.string)
    out.putOpt("description", schema.description, Encode.string)
    out.put("oneOf", schema.oneOf, Encode.vector(EnumOption.encoder))
    out.putOpt("default", schema.default, Encode.string)

  val decoder: Decoder[TitledSingleSelectEnumSchema] = Decode.obj: fields =>
    fields.req("type", Decode.literal("string")).flatMap(_ => read(fields))

  val encoder: Encoder[TitledSingleSelectEnumSchema] = Encode.obj(write)

/** The deprecated way to label a single-select (schema.ts:2431-2448).
  *
  * `enumNames` is required here although the schema marks it optional (ruling M10): without it this
  * member is byte-identical to [[UntitledSingleSelectEnumSchema]], and carrying display names is
  * the only thing it offers over [[TitledSingleSelectEnumSchema]], which the schema says to use
  * instead. See the discrimination table on [[PrimitiveSchemaDefinition]].
  */
final case class LegacyTitledEnumSchema(
  title: Option[String],
  description: Option[String],
  enumValues: Vector[String],
  enumNames: Vector[String],
  default: Option[String]
) extends EnumSchema:
  def schemaType: String = "string"

object LegacyTitledEnumSchema:
  private[mcp] def read(fields: Fields): Either[DecodeError, LegacyTitledEnumSchema] =
    for
      title <- fields.opt("title", Decode.string)
      description <- fields.opt("description", Decode.string)
      enumValues <- fields.req("enum", Decode.vector(Decode.string))
      enumNames <- fields.req("enumNames", Decode.vector(Decode.string))
      default <- fields.opt("default", Decode.string)
    yield LegacyTitledEnumSchema(title, description, enumValues, enumNames, default)

  private[mcp] def write(schema: LegacyTitledEnumSchema, out: JsObj): Unit =
    out.put("type", schema.schemaType, Encode.string)
    out.putOpt("title", schema.title, Encode.string)
    out.putOpt("description", schema.description, Encode.string)
    out.put("enum", schema.enumValues, Encode.vector(Encode.string))
    out.put("enumNames", schema.enumNames, Encode.vector(Encode.string))
    out.putOpt("default", schema.default, Encode.string)

  val decoder: Decoder[LegacyTitledEnumSchema] = Decode.obj: fields =>
    fields.req("type", Decode.literal("string")).flatMap(_ => read(fields))

  val encoder: Encoder[LegacyTitledEnumSchema] = Encode.obj(write)

/** The `items` of an untitled multi-select (schema.ts:2361-2370). */
final case class UntitledMultiSelectItems(enumValues: Vector[String])

object UntitledMultiSelectItems:
  val decoder: Decoder[UntitledMultiSelectItems] = Decode.obj: fields =>
    for
      _ <- fields.req("type", Decode.literal("string"))
      enumValues <- fields.req("enum", Decode.vector(Decode.string))
    yield UntitledMultiSelectItems(enumValues)

  val encoder: Encoder[UntitledMultiSelectItems] = Encode.obj: (items, out) =>
    out.put("type", "string", Encode.string)
    out.put("enum", items.enumValues, Encode.vector(Encode.string))

/** The `items` of a titled multi-select (schema.ts:2400-2417). */
final case class TitledMultiSelectItems(anyOf: Vector[EnumOption])

object TitledMultiSelectItems:
  val decoder: Decoder[TitledMultiSelectItems] = Decode.obj: fields =>
    fields.req("anyOf", Decode.vector(EnumOption.decoder)).map(TitledMultiSelectItems.apply)

  val encoder: Encoder[TitledMultiSelectItems] = Encode.obj: (items, out) =>
    out.put("anyOf", items.anyOf, Encode.vector(EnumOption.encoder))

/** Pick several, values only (schema.ts:2338-2375). */
final case class UntitledMultiSelectEnumSchema(
  title: Option[String],
  description: Option[String],
  minItems: Option[Double],
  maxItems: Option[Double],
  items: UntitledMultiSelectItems,
  default: Option[Vector[String]]
) extends MultiSelectEnumSchema

object UntitledMultiSelectEnumSchema:
  private[mcp] def read(fields: Fields): Either[DecodeError, UntitledMultiSelectEnumSchema] =
    for
      title <- fields.opt("title", Decode.string)
      description <- fields.opt("description", Decode.string)
      minItems <- fields.opt("minItems", Decode.number)
      maxItems <- fields.opt("maxItems", Decode.number)
      items <- fields.req("items", UntitledMultiSelectItems.decoder)
      default <- fields.opt("default", Decode.vector(Decode.string))
    yield UntitledMultiSelectEnumSchema(title, description, minItems, maxItems, items, default)

  private[mcp] def write(schema: UntitledMultiSelectEnumSchema, out: JsObj): Unit =
    out.put("type", schema.schemaType, Encode.string)
    out.putOpt("title", schema.title, Encode.string)
    out.putOpt("description", schema.description, Encode.string)
    out.putOpt("minItems", schema.minItems, Encode.number)
    out.putOpt("maxItems", schema.maxItems, Encode.number)
    out.put("items", schema.items, UntitledMultiSelectItems.encoder)
    out.putOpt("default", schema.default, Encode.vector(Encode.string))

  val decoder: Decoder[UntitledMultiSelectEnumSchema] = Decode.obj: fields =>
    fields.req("type", Decode.literal("array")).flatMap(_ => read(fields))

  val encoder: Encoder[UntitledMultiSelectEnumSchema] = Encode.obj(write)

/** Pick several, with a label for each (schema.ts:2377-2422). */
final case class TitledMultiSelectEnumSchema(
  title: Option[String],
  description: Option[String],
  minItems: Option[Double],
  maxItems: Option[Double],
  items: TitledMultiSelectItems,
  default: Option[Vector[String]]
) extends MultiSelectEnumSchema

object TitledMultiSelectEnumSchema:
  private[mcp] def read(fields: Fields): Either[DecodeError, TitledMultiSelectEnumSchema] =
    for
      title <- fields.opt("title", Decode.string)
      description <- fields.opt("description", Decode.string)
      minItems <- fields.opt("minItems", Decode.number)
      maxItems <- fields.opt("maxItems", Decode.number)
      items <- fields.req("items", TitledMultiSelectItems.decoder)
      default <- fields.opt("default", Decode.vector(Decode.string))
    yield TitledMultiSelectEnumSchema(title, description, minItems, maxItems, items, default)

  private[mcp] def write(schema: TitledMultiSelectEnumSchema, out: JsObj): Unit =
    out.put("type", schema.schemaType, Encode.string)
    out.putOpt("title", schema.title, Encode.string)
    out.putOpt("description", schema.description, Encode.string)
    out.putOpt("minItems", schema.minItems, Encode.number)
    out.putOpt("maxItems", schema.maxItems, Encode.number)
    out.put("items", schema.items, TitledMultiSelectItems.encoder)
    out.putOpt("default", schema.default, Encode.vector(Encode.string))

  val decoder: Decoder[TitledMultiSelectEnumSchema] = Decode.obj: fields =>
    fields.req("type", Decode.literal("array")).flatMap(_ => read(fields))

  val encoder: Encoder[TitledMultiSelectEnumSchema] = Encode.obj(write)

object PrimitiveSchemaDefinition:
  /** The `type` values this union answers to, in schema declaration order. */
  val SchemaTypes: Vector[String] = Vector("string", "number", "integer", "boolean", "array")

  private val expected: String = SchemaTypes.map(Codec.quote).mkString("one of ", ", ", "")

  val decoder: Decoder[PrimitiveSchemaDefinition] = Decode.obj: fields =>
    fields.req("type", Decode.string).flatMap: schemaType =>
      if schemaType == "boolean" then BooleanSchema.read(fields)
      else if schemaType == "number" || schemaType == "integer" then NumberSchema.read(fields)
      else if schemaType == "array" then
        fields
          .req("items", Decode.obj(items => Right(items.present("anyOf"))))
          .flatMap: titled =>
            if titled then TitledMultiSelectEnumSchema.read(fields)
            else UntitledMultiSelectEnumSchema.read(fields)
      else if schemaType == "string" then
        if fields.present("oneOf") then TitledSingleSelectEnumSchema.read(fields)
        else if fields.present("enum") then
          if fields.present("enumNames") then LegacyTitledEnumSchema.read(fields)
          else UntitledSingleSelectEnumSchema.read(fields)
        else StringSchema.read(fields)
      else Left(DecodeError.Mismatch(fields.path / "type", expected, Codec.quote(schemaType)))

  val encoder: Encoder[PrimitiveSchemaDefinition] = Encode.obj: (schema, out) =>
    schema match
      case value: StringSchema                    => StringSchema.write(value, out)
      case value: NumberSchema                    => NumberSchema.write(value, out)
      case value: BooleanSchema                   => BooleanSchema.write(value, out)
      case value: UntitledSingleSelectEnumSchema  => UntitledSingleSelectEnumSchema.write(value, out)
      case value: TitledSingleSelectEnumSchema    => TitledSingleSelectEnumSchema.write(value, out)
      case value: LegacyTitledEnumSchema          => LegacyTitledEnumSchema.write(value, out)
      case value: UntitledMultiSelectEnumSchema   => UntitledMultiSelectEnumSchema.write(value, out)
      case value: TitledMultiSelectEnumSchema     => TitledMultiSelectEnumSchema.write(value, out)

/** The restricted JSON Schema an elicitation form asks to be filled in (schema.ts:2166-2177).
  *
  * Only top-level properties, no nesting — hence [[PrimitiveSchemaDefinition]] rather than a
  * general schema. `properties` is a `Vector` of pairs rather than a `Map` so that the order the
  * server declared its questions in survives; a form whose fields shuffle between sends is a worse
  * form.
  */
final case class RequestedSchema(
  schema: Option[String],
  properties: Vector[(String, PrimitiveSchemaDefinition)],
  required: Option[Vector[String]]
)

object RequestedSchema:
  private val propertiesDecoder: Decoder[Vector[(String, PrimitiveSchemaDefinition)]] =
    (at, value) =>
      if Codec.kindOf(value) != "object" then
        Left(DecodeError.Mismatch(at, "an object", Codec.kindOf(value)))
      else
        val record = value.asInstanceOf[js.Dictionary[js.Any]]
        val keys = js.Object.keys(value.asInstanceOf[js.Object])
        val decoded = Vector.newBuilder[(String, PrimitiveSchemaDefinition)]
        var failure: Option[DecodeError] = None
        var index = 0
        while index < keys.length && failure.isEmpty do
          val key = keys(index)
          PrimitiveSchemaDefinition.decoder.decode(at / key, record(key)) match
            case Right(property) => decoded += (key -> property)
            case Left(reason)    => failure = Some(reason)
          index += 1
        failure.toLeft(decoded.result())

  private val propertiesEncoder: Encoder[Vector[(String, PrimitiveSchemaDefinition)]] =
    properties =>
      val out = new JsObj()
      properties.foreach((key, property) =>
        out.put(key, property, PrimitiveSchemaDefinition.encoder)
      )
      out.result

  val decoder: Decoder[RequestedSchema] = Decode.obj: fields =>
    for
      schema <- fields.opt("$schema", Decode.string)
      _ <- fields.req("type", Decode.literal("object"))
      properties <- fields.req("properties", propertiesDecoder)
      required <- fields.opt("required", Decode.vector(Decode.string))
    yield RequestedSchema(schema, properties, required)

  val encoder: Encoder[RequestedSchema] = Encode.obj: (requested, out) =>
    out.putOpt("$schema", requested.schema, Encode.string)
    out.put("type", "object", Encode.string)
    out.put("properties", requested.properties, propertiesEncoder)
    out.putOpt("required", requested.required, Encode.vector(Encode.string))

/** The params of an `elicitation/create` request (schema.ts:2150-2216).
  *
  * Discriminated by `mode`: absent or `"form"` is a form, `"url"` is a URL, anything else is an
  * error. `mode` is optional only on the form side, and this package always writes it — a message
  * that arrived without one leaves with `"mode":"form"`, which the schema allows and which spares
  * the reader from having to know the default.
  */
sealed trait ElicitRequestParams:
  def meta: Option[Json.Obj]
  def task: Option[TaskMetadata]
  def message: String

  /** The wire `mode` of these params. */
  def mode: String

/** schema.ts:2150-2178. */
final case class ElicitRequestFormParams(
  meta: Option[Json.Obj],
  task: Option[TaskMetadata],
  message: String,
  requestedSchema: RequestedSchema
) extends ElicitRequestParams:
  def mode: String = ElicitRequestFormParams.Mode

object ElicitRequestFormParams:
  val Mode: String = "form"

  private[mcp] def read(fields: Fields): Either[DecodeError, ElicitRequestFormParams] =
    for
      meta <- fields.opt("_meta", Decode.jsonObj)
      task <- fields.opt("task", TaskMetadata.decoder)
      message <- fields.req("message", Decode.string)
      requestedSchema <- fields.req("requestedSchema", RequestedSchema.decoder)
    yield ElicitRequestFormParams(meta, task, message, requestedSchema)

  val decoder: Decoder[ElicitRequestFormParams] = Decode.obj(read)

  val encoder: Encoder[ElicitRequestFormParams] = Encode.obj: (params, out) =>
    out.putOpt("_meta", params.meta, Encode.jsonObj)
    out.putOpt("task", params.task, TaskMetadata.encoder)
    out.put("mode", params.mode, Encode.string)
    out.put("message", params.message, Encode.string)
    out.put("requestedSchema", params.requestedSchema, RequestedSchema.encoder)

/** schema.ts:2180-2208. */
final case class ElicitRequestUrlParams(
  meta: Option[Json.Obj],
  task: Option[TaskMetadata],
  message: String,
  elicitationId: String,
  url: String
) extends ElicitRequestParams:
  def mode: String = ElicitRequestUrlParams.Mode

object ElicitRequestUrlParams:
  val Mode: String = "url"

  private[mcp] def read(fields: Fields): Either[DecodeError, ElicitRequestUrlParams] =
    for
      meta <- fields.opt("_meta", Decode.jsonObj)
      task <- fields.opt("task", TaskMetadata.decoder)
      message <- fields.req("message", Decode.string)
      elicitationId <- fields.req("elicitationId", Decode.string)
      url <- fields.req("url", Decode.string)
    yield ElicitRequestUrlParams(meta, task, message, elicitationId, url)

  val decoder: Decoder[ElicitRequestUrlParams] = Decode.obj: fields =>
    fields.req("mode", Decode.literal(Mode)).flatMap(_ => read(fields))

  val encoder: Encoder[ElicitRequestUrlParams] = Encode.obj: (params, out) =>
    out.putOpt("_meta", params.meta, Encode.jsonObj)
    out.putOpt("task", params.task, TaskMetadata.encoder)
    out.put("mode", params.mode, Encode.string)
    out.put("message", params.message, Encode.string)
    out.put("elicitationId", params.elicitationId, Encode.string)
    out.put("url", params.url, Encode.string)

object ElicitRequestParams:
  private val expected: String =
    Vector(ElicitRequestFormParams.Mode, ElicitRequestUrlParams.Mode)
      .map(Codec.quote)
      .mkString("one of ", ", ", "")

  val decoder: Decoder[ElicitRequestParams] = Decode.obj: fields =>
    fields.opt("mode", Decode.string).flatMap: mode =>
      mode match
        case None | Some(ElicitRequestFormParams.Mode) => ElicitRequestFormParams.read(fields)
        case Some(ElicitRequestUrlParams.Mode)         => ElicitRequestUrlParams.read(fields)
        case Some(other) =>
          Left(DecodeError.Mismatch(fields.path / "mode", expected, Codec.quote(other)))

  val encoder: Encoder[ElicitRequestParams] = value =>
    value match
      case params: ElicitRequestFormParams => ElicitRequestFormParams.encoder.encode(params)
      case params: ElicitRequestUrlParams  => ElicitRequestUrlParams.encoder.encode(params)

/** The client's answer to an elicitation (schema.ts:2457-2477).
  *
  * `content` is present only for an accepted form, and its values are strings, numbers, booleans or
  * arrays of strings — a constraint this package carries rather than enforces, since the shape that
  * satisfies it is whatever [[RequestedSchema]] asked for and validating against a schema is not a
  * codec's job.
  */
final case class ElicitResult(
  meta: Option[Json.Obj],
  action: ElicitAction,
  content: Option[Json.Obj]
)

object ElicitResult:
  val decoder: Decoder[ElicitResult] = Decode.obj: fields =>
    for
      meta <- fields.opt("_meta", Decode.jsonObj)
      action <- fields.req("action", ElicitAction.decoder)
      content <- fields.opt("content", Decode.jsonObj)
    yield ElicitResult(meta, action, content)

  val encoder: Encoder[ElicitResult] = Encode.obj: (result, out) =>
    out.putOpt("_meta", result.meta, Encode.jsonObj)
    out.put("action", result.action, ElicitAction.encoder)
    out.putOpt("content", result.content, Encode.jsonObj)

/** The params of a `notifications/elicitation/complete` notification (schema.ts:2479-2492).
  *
  * The schema writes these params as a bare `{ elicitationId: string }` rather than extending
  * `NotificationParams`, so — unlike every other notification here — they have no `_meta`. Followed
  * as written; a peer that sends one anyway will find it ignored, as unknown fields are everywhere.
  */
final case class ElicitationCompleteNotificationParams(elicitationId: String)

object ElicitationCompleteNotificationParams:
  val decoder: Decoder[ElicitationCompleteNotificationParams] = Decode.obj: fields =>
    fields.req("elicitationId", Decode.string).map(ElicitationCompleteNotificationParams.apply)

  val encoder: Encoder[ElicitationCompleteNotificationParams] = Encode.obj: (params, out) =>
    out.put("elicitationId", params.elicitationId, Encode.string)

/** The `data` of a `-32042` error response (schema.ts:183-199).
  *
  * A server that cannot answer until the user has visited a URL says so with an error rather than a
  * result, and hangs the elicitations it needs off `error.data`. The envelope is an ordinary
  * [[JsonRpc.Error]]; this is the shape its `data` takes, with [[toError]] and [[fromError]] to
  * cross between the two.
  */
final case class UrlElicitationRequiredData(elicitations: Vector[ElicitRequestUrlParams])

object UrlElicitationRequiredData:
  val decoder: Decoder[UrlElicitationRequiredData] = Decode.obj: fields =>
    fields
      .req("elicitations", Decode.vector(ElicitRequestUrlParams.decoder))
      .map(UrlElicitationRequiredData.apply)

  val encoder: Encoder[UrlElicitationRequiredData] = Encode.obj: (data, out) =>
    out.put("elicitations", data.elicitations, Encode.vector(ElicitRequestUrlParams.encoder))

  /** The error a server sends to demand URL elicitation. */
  def toError(message: String, data: UrlElicitationRequiredData): JsonRpc.Error =
    JsonRpc.Error(
      ErrorCode.UrlElicitationRequired,
      message,
      Some(Json.fromJs(encoder.encode(data)))
    )

  /** Reads one back, checking the code first. `at` is where the error sat in the message, so that a
    * complaint points at something the caller can find.
    */
  def fromError(at: Path, error: JsonRpc.Error): Either[DecodeError, UrlElicitationRequiredData] =
    if error.code != ErrorCode.UrlElicitationRequired then
      Left(
        DecodeError.Mismatch(
          at / "code",
          Codec.show(ErrorCode.UrlElicitationRequired),
          Codec.show(error.code)
        )
      )
    else
      error.data match
        case Some(data) => decoder.decode(at / "data", Json.toJs(data))
        case None       => Left(DecodeError.Missing(at / "data"))
