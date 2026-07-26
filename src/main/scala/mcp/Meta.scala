package mcp

/** The small shared records everything else is built out of (schema.ts:459-568, 1017-1022,
  * 1700-1735).
  *
  * TypeScript composes these by interface extension, which flattens on the wire; Scala has no such
  * thing, so [[BaseMetadata]] and [[Icons]] are traits that the concrete records implement and
  * whose fields those records repeat. Encoded field order follows the same flattening: the members
  * an interface inherits come first, in the order of its `extends` clause, then its own.
  */
trait BaseMetadata:
  /** For programs. Also the display name in older revisions, and the fallback when `title` is
    * absent.
    */
  def name: String

  /** For people. */
  def title: Option[String]

/** schema.ts:503-521. */
trait Icons:
  def icons: Option[Vector[Icon]]

/** The background an icon was drawn for (schema.ts:493-500). */
enum IconTheme(val wire: String) extends Wire:
  case Light extends IconTheme("light")
  case Dark extends IconTheme("dark")

object IconTheme:
  val decoder: Decoder[IconTheme] = Decode.wireEnum(IconTheme.values)
  val encoder: Encoder[IconTheme] = Encode.wire

/** An optionally-sized icon for a user interface (schema.ts:459-501). */
final case class Icon(
  src: String,
  mimeType: Option[String],
  sizes: Option[Vector[String]],
  theme: Option[IconTheme]
)

object Icon:
  val decoder: Decoder[Icon] = Decode.obj: fields =>
    for
      src <- fields.req("src", Decode.string)
      mimeType <- fields.opt("mimeType", Decode.string)
      sizes <- fields.opt("sizes", Decode.vector(Decode.string))
      theme <- fields.opt("theme", IconTheme.decoder)
    yield Icon(src, mimeType, sizes, theme)

  val encoder: Encoder[Icon] = Encode.obj: (icon, out) =>
    out.put("src", icon.src, Encode.string)
    out.putOpt("mimeType", icon.mimeType, Encode.string)
    out.putOpt("sizes", icon.sizes, Encode.vector(Encode.string))
    out.putOpt("theme", icon.theme, IconTheme.encoder)

/** Who a message or a piece of content is from or for (schema.ts:1017-1022). */
enum Role(val wire: String) extends Wire:
  case User extends Role("user")
  case Assistant extends Role("assistant")

object Role:
  val decoder: Decoder[Role] = Decode.wireEnum(Role.values)
  val encoder: Encoder[Role] = Encode.wire

/** Hints about how a client should treat a piece of content (schema.ts:1700-1735).
  *
  * `priority` runs 0 to 1 and `lastModified` is an ISO 8601 instant; neither bound is enforced
  * here, since a decoder that rejected an out-of-range advisory hint would be discarding a message
  * over a field nobody has to honour.
  */
final case class Annotations(
  audience: Option[Vector[Role]],
  priority: Option[Double],
  lastModified: Option[String]
)

object Annotations:
  val decoder: Decoder[Annotations] = Decode.obj: fields =>
    for
      audience <- fields.opt("audience", Decode.vector(Role.decoder))
      priority <- fields.opt("priority", Decode.number)
      lastModified <- fields.opt("lastModified", Decode.string)
    yield Annotations(audience, priority, lastModified)

  val encoder: Encoder[Annotations] = Encode.obj: (annotations, out) =>
    out.putOpt("audience", annotations.audience, Encode.vector(Role.encoder))
    out.putOpt("priority", annotations.priority, Encode.number)
    out.putOpt("lastModified", annotations.lastModified, Encode.string)

/** Who is on the other end of a session (schema.ts:545-568). */
final case class Implementation(
  name: String,
  title: Option[String],
  icons: Option[Vector[Icon]],
  version: String,
  description: Option[String],
  websiteUrl: Option[String]
) extends BaseMetadata,
      Icons

object Implementation:
  val decoder: Decoder[Implementation] = Decode.obj: fields =>
    for
      name <- fields.req("name", Decode.string)
      title <- fields.opt("title", Decode.string)
      icons <- fields.opt("icons", Decode.vector(Icon.decoder))
      version <- fields.req("version", Decode.string)
      description <- fields.opt("description", Decode.string)
      websiteUrl <- fields.opt("websiteUrl", Decode.string)
    yield Implementation(name, title, icons, version, description, websiteUrl)

  val encoder: Encoder[Implementation] = Encode.obj: (implementation, out) =>
    out.put("name", implementation.name, Encode.string)
    out.putOpt("title", implementation.title, Encode.string)
    out.putOpt("icons", implementation.icons, Encode.vector(Icon.encoder))
    out.put("version", implementation.version, Encode.string)
    out.putOpt("description", implementation.description, Encode.string)
    out.putOpt("websiteUrl", implementation.websiteUrl, Encode.string)
