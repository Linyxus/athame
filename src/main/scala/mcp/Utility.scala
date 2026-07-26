package mcp

/** The three messages that are about the conversation rather than its subject
  * (schema.ts:209-247, 570-619).
  *
  * `ping` carries nothing but [[RequestParams]] and so has no params type of its own; it appears
  * only as a case of [[ClientRequest]] and [[ServerRequest]].
  */

/** The params of a `notifications/cancelled` notification (schema.ts:209-229).
  *
  * `requestId` is optional in the schema even though the prose requires it for cancelling a
  * non-task request — the field is optional because task cancellation goes through `tasks/cancel`
  * instead, which this package does not model. Decoding follows the schema, not the prose.
  */
final case class CancelledNotificationParams(
  meta: Option[Json.Obj],
  requestId: Option[RequestId],
  reason: Option[String]
)

object CancelledNotificationParams:
  val decoder: Decoder[CancelledNotificationParams] = Decode.obj: fields =>
    for
      meta <- fields.opt("_meta", Decode.jsonObj)
      requestId <- fields.opt("requestId", RequestId.decoder)
      reason <- fields.opt("reason", Decode.string)
    yield CancelledNotificationParams(meta, requestId, reason)

  val encoder: Encoder[CancelledNotificationParams] = Encode.obj: (params, out) =>
    out.putOpt("_meta", params.meta, Encode.jsonObj)
    out.putOpt("requestId", params.requestId, RequestId.encoder)
    out.putOpt("reason", params.reason, Encode.string)

/** The params of a `notifications/progress` notification (schema.ts:581-609).
  *
  * `progressToken` here is a top-level field of the notification, not a `_meta` entry: the token
  * goes *into* `_meta` on the request that asks for progress (see [[ProgressToken.into]]) and comes
  * back *out here* on every update.
  */
final case class ProgressNotificationParams(
  meta: Option[Json.Obj],
  progressToken: ProgressToken,
  progress: Double,
  total: Option[Double],
  message: Option[String]
)

object ProgressNotificationParams:
  val decoder: Decoder[ProgressNotificationParams] = Decode.obj: fields =>
    for
      meta <- fields.opt("_meta", Decode.jsonObj)
      progressToken <- fields.req("progressToken", ProgressToken.decoder)
      progress <- fields.req("progress", Decode.number)
      total <- fields.opt("total", Decode.number)
      message <- fields.opt("message", Decode.string)
    yield ProgressNotificationParams(meta, progressToken, progress, total, message)

  val encoder: Encoder[ProgressNotificationParams] = Encode.obj: (params, out) =>
    out.putOpt("_meta", params.meta, Encode.jsonObj)
    out.put("progressToken", params.progressToken, ProgressToken.encoder)
    out.put("progress", params.progress, Encode.number)
    out.putOpt("total", params.total, Encode.number)
    out.putOpt("message", params.message, Encode.string)
