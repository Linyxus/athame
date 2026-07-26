package mcp

/** Log levels and the two messages that use them (schema.ts:1500-1570). */

/** The eight RFC-5424 severities, ordered as the schema orders them: least severe first
  * (schema.ts:1554-1570).
  *
  * A client asking for `Warning` is asking for warnings *and everything above them*, so the
  * declaration order is load-bearing beyond spelling — hence [[LoggingLevel.severity]].
  */
enum LoggingLevel(val wire: String) extends Wire:
  case Debug extends LoggingLevel("debug")
  case Info extends LoggingLevel("info")
  case Notice extends LoggingLevel("notice")
  case Warning extends LoggingLevel("warning")
  case Error extends LoggingLevel("error")
  case Critical extends LoggingLevel("critical")
  case Alert extends LoggingLevel("alert")
  case Emergency extends LoggingLevel("emergency")

  /** How severe this level is, counting up. `Debug` is 0 and `Emergency` is 7, matching the order
    * the schema lists and the direction RFC 5424 means by "this level and higher".
    */
  def severity: Int = ordinal

object LoggingLevel:
  val decoder: Decoder[LoggingLevel] = Decode.wireEnum(LoggingLevel.values)
  val encoder: Encoder[LoggingLevel] = Encode.wire

/** The params of a `logging/setLevel` request (schema.ts:1502-1512). */
final case class SetLevelRequestParams(meta: Option[Json.Obj], level: LoggingLevel)

object SetLevelRequestParams:
  val decoder: Decoder[SetLevelRequestParams] = Decode.obj: fields =>
    for
      meta <- fields.opt("_meta", Decode.jsonObj)
      level <- fields.req("level", LoggingLevel.decoder)
    yield SetLevelRequestParams(meta, level)

  val encoder: Encoder[SetLevelRequestParams] = Encode.obj: (params, out) =>
    out.putOpt("_meta", params.meta, Encode.jsonObj)
    out.put("level", params.level, LoggingLevel.encoder)

/** The params of a `notifications/message` notification (schema.ts:1524-1542).
  *
  * `data` is required and unconstrained — a string, an object, anything JSON can hold — so it is
  * carried as [[Json]], and `"data": null` is the value `null` rather than an absence.
  */
final case class LoggingMessageNotificationParams(
  meta: Option[Json.Obj],
  level: LoggingLevel,
  logger: Option[String],
  data: Json
)

object LoggingMessageNotificationParams:
  val decoder: Decoder[LoggingMessageNotificationParams] = Decode.obj: fields =>
    for
      meta <- fields.opt("_meta", Decode.jsonObj)
      level <- fields.req("level", LoggingLevel.decoder)
      logger <- fields.opt("logger", Decode.string)
      data <- fields.req("data", Decode.json)
    yield LoggingMessageNotificationParams(meta, level, logger, data)

  val encoder: Encoder[LoggingMessageNotificationParams] = Encode.obj: (params, out) =>
    out.putOpt("_meta", params.meta, Encode.jsonObj)
    out.put("level", params.level, LoggingLevel.encoder)
    out.putOpt("logger", params.logger, Encode.string)
    out.put("data", params.data, Encode.json)
