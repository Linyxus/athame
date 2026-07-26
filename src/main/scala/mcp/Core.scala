package mcp

import scala.scalajs.js

/** The Model Context Protocol, revision 2025-11-25, as datatypes and codecs.
  *
  * Every type here gets both a decoder and an encoder, whichever side of a session sends it: athame
  * may end up as the client or as the server, and a package that only knew how to read what servers
  * say would have to be written twice. What is *not* here is any notion of a session — no transport,
  * no request/response correlation, no dispatch loop. This layer turns bytes into values.
  *
  * The field-level authority is the published `schema.ts` for revision 2025-11-25; each file below
  * cites the line ranges it models, and where a name could not survive the trip into Scala it is
  * mapped uniformly:
  *
  *   - `_meta` on the wire is `meta` in Scala,
  *   - `$schema` is `schema` (`$` belongs to the compiler),
  *   - `enum` is `enumValues` (a Scala 3 keyword).
  *
  * Deliberately out of scope, and not merely unimplemented: the resources, prompts, roots and
  * completion protocol operations; the whole tasks request family (`tasks/get`, `tasks/result`,
  * `tasks/cancel`, `tasks/list`, `notifications/tasks/status`, and the `Task`, `TaskStatus` and
  * `CreateTaskResult` datatypes); every transport; and authorization. Task *augmentation* survives
  * as data — [[TaskMetadata]] and [[RelatedTaskMetadata]] — because `tools/call`, sampling and
  * elicitation params carry it and dropping the field would corrupt messages we do handle.
  * [[Resource]] is likewise present as a record, since `resource_link` content embeds one, while
  * `resources/list` and friends are not.
  */
object Mcp:
  /** schema.ts:12 — the revision this package models. */
  val LatestProtocolVersion: String = "2025-11-25"

/** The JSON-RPC layer MCP rides on (schema.ts:1-207).
  *
  * There is no batching: it was removed from MCP in revision 2025-06-18 and 2025-11-25 does not
  * bring it back, so a top-level array is a decode error rather than a list of messages.
  */
object JsonRpc:
  /** schema.ts:14 — the only value the `jsonrpc` field may take. */
  val Version: String = "2.0"

  /** The `error` member of an error response (schema.ts:99-115).
    *
    * `data` is the one optional field in the package that can hold `null` as a *value*, which the
    * wire cannot tell from an absence once the leniency rule has read `"data":null` as `None`. The
    * two are therefore the same message: `Some(Json.Null)` encodes to no `data` member at all, and
    * comes back as `None`.
    */
  final case class Error(code: Double, message: String, data: Option[Json])

  object Error:
    val decoder: Decoder[Error] = Decode.obj: fields =>
      for
        code <- fields.req("code", Decode.number)
        message <- fields.req("message", Decode.string)
        data <- fields.opt("data", Decode.json)
      yield Error(code, message, data)

    val encoder: Encoder[Error] = Encode.obj: (error, out) =>
      out.put("code", error.code, Encode.number)
      out.put("message", error.message, Encode.string)
      out.putJson("data", error.data)

/** The error codes MCP names (schema.ts:172-199).
  *
  * `Double` rather than `Int` because every number in this package is a JavaScript number; see the
  * note on [[JsonRpc.Error]]'s `code`.
  */
object ErrorCode:
  val ParseError: Double = -32700
  val InvalidRequest: Double = -32600
  val MethodNotFound: Double = -32601
  val InvalidParams: Double = -32602
  val InternalError: Double = -32603

  /** Implementation-specific: the server needs the client to visit a URL before it can answer
    * (schema.ts:181). See [[UrlElicitationRequiredData]].
    */
  val UrlElicitationRequired: Double = -32042

/** A request identifier: a string or a number, and it round-trips as whichever it arrived as
  * (schema.ts:117-122).
  */
enum RequestId:
  case Str(value: String)
  case Num(value: Double)

object RequestId:
  val decoder: Decoder[RequestId] = (at, value) =>
    js.typeOf(value) match
      case "string" => Right(RequestId.Str(value.asInstanceOf[String]))
      case "number" => Right(RequestId.Num(value.asInstanceOf[Double]))
      case _ => Left(DecodeError.Mismatch(at, "a string or number", Codec.kindOf(value)))

  val encoder: Encoder[RequestId] = value =>
    value match
      case RequestId.Str(text)   => Encode.string.encode(text)
      case RequestId.Num(number) => Encode.number.encode(number)

/** The token that ties `notifications/progress` back to the request that is progressing
  * (schema.ts:16-21).
  */
enum ProgressToken:
  case Str(value: String)
  case Num(value: Double)

object ProgressToken:
  val decoder: Decoder[ProgressToken] = (at, value) =>
    js.typeOf(value) match
      case "string" => Right(ProgressToken.Str(value.asInstanceOf[String]))
      case "number" => Right(ProgressToken.Num(value.asInstanceOf[Double]))
      case _ => Left(DecodeError.Mismatch(at, "a string or number", Codec.kindOf(value)))

  val encoder: Encoder[ProgressToken] = value =>
    value match
      case ProgressToken.Str(text)   => Encode.string.encode(text)
      case ProgressToken.Num(number) => Encode.number.encode(number)

  /** The one typed member the spec defines inside a request's `_meta` (schema.ts:51-62).
    *
    * `_meta` is otherwise carried verbatim, so reading and writing this key is a pair of helpers
    * rather than a field: a request that never asked for progress has no business growing a
    * `Some(...)`-shaped hole in its params.
    */
  val MetaKey: String = "progressToken"

  /** The progress token inside a params `_meta`, if it is there and is a string or a number. */
  def from(meta: Option[Json.Obj]): Option[ProgressToken] =
    meta.flatMap(_.get(MetaKey)).flatMap:
      case Json.Str(text)   => Some(ProgressToken.Str(text))
      case Json.Num(number) => Some(ProgressToken.Num(number))
      case _                => None

  /** `meta` with `progressToken` set, replacing any existing entry in place and appending
    * otherwise, so the rest of the caller's `_meta` keeps both its contents and its order.
    */
  def into(meta: Option[Json.Obj], token: ProgressToken): Json.Obj =
    val written = token match
      case ProgressToken.Str(text)   => Json.Str(text)
      case ProgressToken.Num(number) => Json.Num(number)
    val fields = meta.map(_.fields).getOrElse(Vector.empty)
    if fields.exists((key, _) => key == MetaKey) then
      Json.Obj(fields.map((key, value) => if key == MetaKey then key -> written else key -> value))
    else Json.Obj(fields :+ (MetaKey -> written))

/** An opaque pagination cursor (schema.ts:23-28). A string, and never inspected by us. */
type Cursor = String

/** The params every request may carry (schema.ts:46-62). */
final case class RequestParams(meta: Option[Json.Obj])

object RequestParams:
  val empty: RequestParams = RequestParams(None)

  val decoder: Decoder[RequestParams] = Decode.obj: fields =>
    fields.opt("_meta", Decode.jsonObj).map(RequestParams.apply)

  val encoder: Encoder[RequestParams] = Encode.obj: (params, out) =>
    out.putOpt("_meta", params.meta, Encode.jsonObj)

/** The params every notification may carry (schema.ts:72-78). */
final case class NotificationParams(meta: Option[Json.Obj])

object NotificationParams:
  val empty: NotificationParams = NotificationParams(None)

  val decoder: Decoder[NotificationParams] = Decode.obj: fields =>
    fields.opt("_meta", Decode.jsonObj).map(NotificationParams.apply)

  val encoder: Encoder[NotificationParams] = Encode.obj: (params, out) =>
    out.putOpt("_meta", params.meta, Encode.jsonObj)

/** The params a paginated request may carry (schema.ts:621-638).
  *
  * Pagination as a utility family is out of scope; the cursor fields are in, because `tools/list`
  * would be wrong without them.
  */
final case class PaginatedRequestParams(meta: Option[Json.Obj], cursor: Option[Cursor])

object PaginatedRequestParams:
  val empty: PaginatedRequestParams = PaginatedRequestParams(None, None)

  val decoder: Decoder[PaginatedRequestParams] = Decode.obj: fields =>
    for
      meta <- fields.opt("_meta", Decode.jsonObj)
      cursor <- fields.opt("cursor", Decode.string)
    yield PaginatedRequestParams(meta, cursor)

  val encoder: Encoder[PaginatedRequestParams] = Encode.obj: (params, out) =>
    out.putOpt("_meta", params.meta, Encode.jsonObj)
    out.putOpt("cursor", params.cursor, Encode.string)

/** Success with nothing to say (schema.ts:201-207).
  *
  * A case class rather than the case object the design note asked for, because the schema gives
  * `EmptyResult` the same `_meta` every other result has and a singleton cannot carry one.
  * [[EmptyResult.empty]] is the value that note was reaching for.
  *
  * `Result`'s index signature — the schema lets any result carry extra top-level members — is not
  * modelled anywhere in this package: decoders ignore what they do not know, encoders write only
  * the fields they have.
  */
final case class EmptyResult(meta: Option[Json.Obj])

object EmptyResult:
  val empty: EmptyResult = EmptyResult(None)

  val decoder: Decoder[EmptyResult] = Decode.obj: fields =>
    fields.opt("_meta", Decode.jsonObj).map(EmptyResult.apply)

  val encoder: Encoder[EmptyResult] = Encode.obj: (result, out) =>
    out.putOpt("_meta", result.meta, Encode.jsonObj)

/** The `task` field of a task-augmented request (schema.ts:1313-1324).
  *
  * The tasks family itself is out of scope. This type is in because `tools/call`,
  * `sampling/createMessage` and `elicitation/create` params may carry it, and a decoder that
  * dropped the field would silently turn a task-augmented call into an ordinary one.
  */
final case class TaskMetadata(ttl: Option[Double])

object TaskMetadata:
  val decoder: Decoder[TaskMetadata] = Decode.obj: fields =>
    fields.opt("ttl", Decode.number).map(TaskMetadata.apply)

  val encoder: Encoder[TaskMetadata] = Encode.obj: (task, out) =>
    out.putOpt("ttl", task.ttl, Encode.number)

/** The `_meta` entry that associates a message with a task (schema.ts:1326-1337).
  *
  * Lives under [[RelatedTaskMetadata.MetaKey]] in any `_meta`, which this package carries verbatim
  * — so this is a codec you reach for, not one the message decoders run for you.
  */
final case class RelatedTaskMetadata(taskId: String)

object RelatedTaskMetadata:
  /** schema.ts:1328 — the well-known `_meta` key. */
  val MetaKey: String = "io.modelcontextprotocol/related-task"

  val decoder: Decoder[RelatedTaskMetadata] = Decode.obj: fields =>
    fields.req("taskId", Decode.string).map(RelatedTaskMetadata.apply)

  val encoder: Encoder[RelatedTaskMetadata] = Encode.obj: (related, out) =>
    out.put("taskId", related.taskId, Encode.string)
