package mcp

/** Content blocks and the resource records they carry (schema.ts:797-919, 1044-1069, 1737-1908).
  *
  * Two unions live here. [[ContentBlock]] is what prompts and tool results are made of;
  * [[SamplingMessageContentBlock]] is what a sampling conversation is made of. They overlap —
  * text, image and audio belong to both — so [[ToolUseContent]] and [[ToolResultContent]] are in
  * this file rather than beside the rest of sampling: Scala seals a trait only over the children
  * declared alongside it, and sealing is what makes every match over these unions exhaustive.
  *
  * Both unions discriminate on the wire `type` field, which is also the first field every block
  * encodes. A `type` neither union recognises is a decode error naming it, never a default member;
  * silently reading an unknown block as text would put words in a model's mouth.
  */
sealed trait ContentBlock:
  /** The wire `type` of this block. */
  def tag: String

/** schema.ts:1690-1698. */
sealed trait SamplingMessageContentBlock:
  /** The wire `type` of this block. */
  def tag: String

/** A resource the server can read (schema.ts:797-838).
  *
  * The `resources/list` and `resources/read` operations are out of scope; this record is in because
  * [[ResourceLink]] is a resource with a `type` bolted on, and tool results hand them out.
  */
final case class Resource(
  name: String,
  title: Option[String],
  icons: Option[Vector[Icon]],
  uri: String,
  description: Option[String],
  mimeType: Option[String],
  annotations: Option[Annotations],
  size: Option[Double],
  meta: Option[Json.Obj]
) extends BaseMetadata,
      Icons

object Resource:
  private[mcp] def read(fields: Fields): Either[DecodeError, Resource] =
    for
      name <- fields.req("name", Decode.string)
      title <- fields.opt("title", Decode.string)
      icons <- fields.opt("icons", Decode.vector(Icon.decoder))
      uri <- fields.req("uri", Decode.string)
      description <- fields.opt("description", Decode.string)
      mimeType <- fields.opt("mimeType", Decode.string)
      annotations <- fields.opt("annotations", Annotations.decoder)
      size <- fields.opt("size", Decode.number)
      meta <- fields.opt("_meta", Decode.jsonObj)
    yield Resource(name, title, icons, uri, description, mimeType, annotations, size, meta)

  private[mcp] def write(resource: Resource, out: JsObj): Unit =
    out.put("name", resource.name, Encode.string)
    out.putOpt("title", resource.title, Encode.string)
    out.putOpt("icons", resource.icons, Encode.vector(Icon.encoder))
    out.put("uri", resource.uri, Encode.string)
    out.putOpt("description", resource.description, Encode.string)
    out.putOpt("mimeType", resource.mimeType, Encode.string)
    out.putOpt("annotations", resource.annotations, Annotations.encoder)
    out.putOpt("size", resource.size, Encode.number)
    out.putOpt("_meta", resource.meta, Encode.jsonObj)

  val decoder: Decoder[Resource] = Decode.obj(read)
  val encoder: Encoder[Resource] = Encode.obj(write)

/** The bytes of a resource, as text or as base64 (schema.ts:876-919).
  *
  * Discriminated by which of `text` and `blob` is present, since the schema gives these two no
  * `type` field. `text` is checked first; an object carrying both is read as text rather than
  * refused, because a sender who supplied both told us the same thing twice.
  */
sealed trait ResourceContents:
  def uri: String
  def mimeType: Option[String]
  def meta: Option[Json.Obj]

/** schema.ts:899-907. */
final case class TextResourceContents(
  uri: String,
  mimeType: Option[String],
  meta: Option[Json.Obj],
  text: String
) extends ResourceContents

object TextResourceContents:
  private[mcp] def read(fields: Fields): Either[DecodeError, TextResourceContents] =
    for
      uri <- fields.req("uri", Decode.string)
      mimeType <- fields.opt("mimeType", Decode.string)
      meta <- fields.opt("_meta", Decode.jsonObj)
      text <- fields.req("text", Decode.string)
    yield TextResourceContents(uri, mimeType, meta, text)

  val decoder: Decoder[TextResourceContents] = Decode.obj(read)

  val encoder: Encoder[TextResourceContents] = Encode.obj: (contents, out) =>
    out.put("uri", contents.uri, Encode.string)
    out.putOpt("mimeType", contents.mimeType, Encode.string)
    out.putOpt("_meta", contents.meta, Encode.jsonObj)
    out.put("text", contents.text, Encode.string)

/** schema.ts:909-919. */
final case class BlobResourceContents(
  uri: String,
  mimeType: Option[String],
  meta: Option[Json.Obj],
  blob: String
) extends ResourceContents

object BlobResourceContents:
  private[mcp] def read(fields: Fields): Either[DecodeError, BlobResourceContents] =
    for
      uri <- fields.req("uri", Decode.string)
      mimeType <- fields.opt("mimeType", Decode.string)
      meta <- fields.opt("_meta", Decode.jsonObj)
      blob <- fields.req("blob", Decode.string)
    yield BlobResourceContents(uri, mimeType, meta, blob)

  val decoder: Decoder[BlobResourceContents] = Decode.obj(read)

  val encoder: Encoder[BlobResourceContents] = Encode.obj: (contents, out) =>
    out.put("uri", contents.uri, Encode.string)
    out.putOpt("mimeType", contents.mimeType, Encode.string)
    out.putOpt("_meta", contents.meta, Encode.jsonObj)
    out.put("blob", contents.blob, Encode.string)

object ResourceContents:
  val decoder: Decoder[ResourceContents] = Decode.obj: fields =>
    if fields.present("text") then TextResourceContents.read(fields)
    else if fields.present("blob") then BlobResourceContents.read(fields)
    else
      Left(DecodeError.Mismatch(fields.path, "an object with \"text\" or \"blob\"", "object"))

  val encoder: Encoder[ResourceContents] = value =>
    value match
      case contents: TextResourceContents => TextResourceContents.encoder.encode(contents)
      case contents: BlobResourceContents => BlobResourceContents.encoder.encode(contents)

/** Text to or from a model (schema.ts:1743-1765). */
final case class TextContent(
  text: String,
  annotations: Option[Annotations],
  meta: Option[Json.Obj]
) extends ContentBlock,
      SamplingMessageContentBlock:
  def tag: String = TextContent.Tag

object TextContent:
  val Tag: String = "text"

  private[mcp] def read(fields: Fields): Either[DecodeError, TextContent] =
    for
      text <- fields.req("text", Decode.string)
      annotations <- fields.opt("annotations", Annotations.decoder)
      meta <- fields.opt("_meta", Decode.jsonObj)
    yield TextContent(text, annotations, meta)

  val decoder: Decoder[TextContent] = Decode.obj: fields =>
    fields.req("type", Decode.literal(Tag)).flatMap(_ => read(fields))

  val encoder: Encoder[TextContent] = Encode.obj: (content, out) =>
    out.put("type", content.tag, Encode.string)
    out.put("text", content.text, Encode.string)
    out.putOpt("annotations", content.annotations, Annotations.encoder)
    out.putOpt("_meta", content.meta, Encode.jsonObj)

/** An image to or from a model (schema.ts:1767-1796). */
final case class ImageContent(
  data: String,
  mimeType: String,
  annotations: Option[Annotations],
  meta: Option[Json.Obj]
) extends ContentBlock,
      SamplingMessageContentBlock:
  def tag: String = ImageContent.Tag

object ImageContent:
  val Tag: String = "image"

  private[mcp] def read(fields: Fields): Either[DecodeError, ImageContent] =
    for
      data <- fields.req("data", Decode.string)
      mimeType <- fields.req("mimeType", Decode.string)
      annotations <- fields.opt("annotations", Annotations.decoder)
      meta <- fields.opt("_meta", Decode.jsonObj)
    yield ImageContent(data, mimeType, annotations, meta)

  val decoder: Decoder[ImageContent] = Decode.obj: fields =>
    fields.req("type", Decode.literal(Tag)).flatMap(_ => read(fields))

  val encoder: Encoder[ImageContent] = Encode.obj: (content, out) =>
    out.put("type", content.tag, Encode.string)
    out.put("data", content.data, Encode.string)
    out.put("mimeType", content.mimeType, Encode.string)
    out.putOpt("annotations", content.annotations, Annotations.encoder)
    out.putOpt("_meta", content.meta, Encode.jsonObj)

/** Audio to or from a model (schema.ts:1798-1827). */
final case class AudioContent(
  data: String,
  mimeType: String,
  annotations: Option[Annotations],
  meta: Option[Json.Obj]
) extends ContentBlock,
      SamplingMessageContentBlock:
  def tag: String = AudioContent.Tag

object AudioContent:
  val Tag: String = "audio"

  private[mcp] def read(fields: Fields): Either[DecodeError, AudioContent] =
    for
      data <- fields.req("data", Decode.string)
      mimeType <- fields.req("mimeType", Decode.string)
      annotations <- fields.opt("annotations", Annotations.decoder)
      meta <- fields.opt("_meta", Decode.jsonObj)
    yield AudioContent(data, mimeType, annotations, meta)

  val decoder: Decoder[AudioContent] = Decode.obj: fields =>
    fields.req("type", Decode.literal(Tag)).flatMap(_ => read(fields))

  val encoder: Encoder[AudioContent] = Encode.obj: (content, out) =>
    out.put("type", content.tag, Encode.string)
    out.put("data", content.data, Encode.string)
    out.put("mimeType", content.mimeType, Encode.string)
    out.putOpt("annotations", content.annotations, Annotations.encoder)
    out.putOpt("_meta", content.meta, Encode.jsonObj)

/** A pointer to a resource, handed out inside a prompt or a tool result (schema.ts:1037-1046).
  *
  * The schema writes this as `Resource` plus a `type`, so the fields are [[Resource]]'s, repeated
  * here rather than nested: that is what the wire looks like, and `link.uri` is what a caller
  * wants to write. [[resource]] recovers the record when one is needed.
  */
final case class ResourceLink(
  name: String,
  title: Option[String],
  icons: Option[Vector[Icon]],
  uri: String,
  description: Option[String],
  mimeType: Option[String],
  annotations: Option[Annotations],
  size: Option[Double],
  meta: Option[Json.Obj]
) extends ContentBlock,
      BaseMetadata,
      Icons:
  def tag: String = ResourceLink.Tag

  def resource: Resource =
    Resource(name, title, icons, uri, description, mimeType, annotations, size, meta)

object ResourceLink:
  val Tag: String = "resource_link"

  def apply(resource: Resource): ResourceLink =
    ResourceLink(
      resource.name,
      resource.title,
      resource.icons,
      resource.uri,
      resource.description,
      resource.mimeType,
      resource.annotations,
      resource.size,
      resource.meta
    )

  private[mcp] def read(fields: Fields): Either[DecodeError, ResourceLink] =
    Resource.read(fields).map(ResourceLink.apply)

  val decoder: Decoder[ResourceLink] = Decode.obj: fields =>
    fields.req("type", Decode.literal(Tag)).flatMap(_ => read(fields))

  val encoder: Encoder[ResourceLink] = Encode.obj: (link, out) =>
    out.put("type", link.tag, Encode.string)
    Resource.write(link.resource, out)

/** A resource's contents, inlined rather than linked (schema.ts:1048-1069). */
final case class EmbeddedResource(
  resource: ResourceContents,
  annotations: Option[Annotations],
  meta: Option[Json.Obj]
) extends ContentBlock:
  def tag: String = EmbeddedResource.Tag

object EmbeddedResource:
  val Tag: String = "resource"

  private[mcp] def read(fields: Fields): Either[DecodeError, EmbeddedResource] =
    for
      resource <- fields.req("resource", ResourceContents.decoder)
      annotations <- fields.opt("annotations", Annotations.decoder)
      meta <- fields.opt("_meta", Decode.jsonObj)
    yield EmbeddedResource(resource, annotations, meta)

  val decoder: Decoder[EmbeddedResource] = Decode.obj: fields =>
    fields.req("type", Decode.literal(Tag)).flatMap(_ => read(fields))

  val encoder: Encoder[EmbeddedResource] = Encode.obj: (embedded, out) =>
    out.put("type", embedded.tag, Encode.string)
    out.put("resource", embedded.resource, ResourceContents.encoder)
    out.putOpt("annotations", embedded.annotations, Annotations.encoder)
    out.putOpt("_meta", embedded.meta, Encode.jsonObj)

/** A model asking for a tool to be called, mid-conversation (schema.ts:1829-1861). */
final case class ToolUseContent(
  id: String,
  name: String,
  input: Json.Obj,
  meta: Option[Json.Obj]
) extends SamplingMessageContentBlock:
  def tag: String = ToolUseContent.Tag

object ToolUseContent:
  val Tag: String = "tool_use"

  private[mcp] def read(fields: Fields): Either[DecodeError, ToolUseContent] =
    for
      id <- fields.req("id", Decode.string)
      name <- fields.req("name", Decode.string)
      input <- fields.req("input", Decode.jsonObj)
      meta <- fields.opt("_meta", Decode.jsonObj)
    yield ToolUseContent(id, name, input, meta)

  val decoder: Decoder[ToolUseContent] = Decode.obj: fields =>
    fields.req("type", Decode.literal(Tag)).flatMap(_ => read(fields))

  val encoder: Encoder[ToolUseContent] = Encode.obj: (content, out) =>
    out.put("type", content.tag, Encode.string)
    out.put("id", content.id, Encode.string)
    out.put("name", content.name, Encode.string)
    out.put("input", content.input, Encode.jsonObj)
    out.putOpt("_meta", content.meta, Encode.jsonObj)

/** What came back from that tool, handed to the model (schema.ts:1863-1908). */
final case class ToolResultContent(
  toolUseId: String,
  content: Vector[ContentBlock],
  structuredContent: Option[Json.Obj],
  isError: Option[Boolean],
  meta: Option[Json.Obj]
) extends SamplingMessageContentBlock:
  def tag: String = ToolResultContent.Tag

object ToolResultContent:
  val Tag: String = "tool_result"

  private[mcp] def read(fields: Fields): Either[DecodeError, ToolResultContent] =
    for
      toolUseId <- fields.req("toolUseId", Decode.string)
      content <- fields.req("content", Decode.vector(ContentBlock.decoder))
      structuredContent <- fields.opt("structuredContent", Decode.jsonObj)
      isError <- fields.opt("isError", Decode.boolean)
      meta <- fields.opt("_meta", Decode.jsonObj)
    yield ToolResultContent(toolUseId, content, structuredContent, isError, meta)

  val decoder: Decoder[ToolResultContent] = Decode.obj: fields =>
    fields.req("type", Decode.literal(Tag)).flatMap(_ => read(fields))

  val encoder: Encoder[ToolResultContent] = Encode.obj: (result, out) =>
    out.put("type", result.tag, Encode.string)
    out.put("toolUseId", result.toolUseId, Encode.string)
    out.put("content", result.content, Encode.vector(ContentBlock.encoder))
    out.putOpt("structuredContent", result.structuredContent, Encode.jsonObj)
    out.putOpt("isError", result.isError, Encode.boolean)
    out.putOpt("_meta", result.meta, Encode.jsonObj)

object ContentBlock:
  /** The `type` values this union answers to, in schema declaration order. */
  val Tags: Vector[String] =
    Vector(TextContent.Tag, ImageContent.Tag, AudioContent.Tag, ResourceLink.Tag, EmbeddedResource.Tag)

  private val expected: String = Tags.map(Codec.quote).mkString("one of ", ", ", "")

  val decoder: Decoder[ContentBlock] = Decode.obj: fields =>
    fields.req("type", Decode.string).flatMap: tag =>
      if tag == TextContent.Tag then TextContent.read(fields)
      else if tag == ImageContent.Tag then ImageContent.read(fields)
      else if tag == AudioContent.Tag then AudioContent.read(fields)
      else if tag == ResourceLink.Tag then ResourceLink.read(fields)
      else if tag == EmbeddedResource.Tag then EmbeddedResource.read(fields)
      else Left(DecodeError.Mismatch(fields.path / "type", expected, Codec.quote(tag)))

  val encoder: Encoder[ContentBlock] = value =>
    value match
      case block: TextContent      => TextContent.encoder.encode(block)
      case block: ImageContent     => ImageContent.encoder.encode(block)
      case block: AudioContent     => AudioContent.encoder.encode(block)
      case block: ResourceLink     => ResourceLink.encoder.encode(block)
      case block: EmbeddedResource => EmbeddedResource.encoder.encode(block)

object SamplingMessageContentBlock:
  /** The `type` values this union answers to, in schema declaration order. */
  val Tags: Vector[String] =
    Vector(TextContent.Tag, ImageContent.Tag, AudioContent.Tag, ToolUseContent.Tag, ToolResultContent.Tag)

  private val expected: String = Tags.map(Codec.quote).mkString("one of ", ", ", "")

  val decoder: Decoder[SamplingMessageContentBlock] = Decode.obj: fields =>
    fields.req("type", Decode.string).flatMap: tag =>
      if tag == TextContent.Tag then TextContent.read(fields)
      else if tag == ImageContent.Tag then ImageContent.read(fields)
      else if tag == AudioContent.Tag then AudioContent.read(fields)
      else if tag == ToolUseContent.Tag then ToolUseContent.read(fields)
      else if tag == ToolResultContent.Tag then ToolResultContent.read(fields)
      else Left(DecodeError.Mismatch(fields.path / "type", expected, Codec.quote(tag)))

  val encoder: Encoder[SamplingMessageContentBlock] = value =>
    value match
      case block: TextContent       => TextContent.encoder.encode(block)
      case block: ImageContent      => ImageContent.encoder.encode(block)
      case block: AudioContent      => AudioContent.encoder.encode(block)
      case block: ToolUseContent    => ToolUseContent.encoder.encode(block)
      case block: ToolResultContent => ToolResultContent.encoder.encode(block)
