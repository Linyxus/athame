package mcp

import scala.scalajs.js

/** `sampling/createMessage`: a server borrowing the client's model (schema.ts:1572-1698,
  * 1910-1992).
  *
  * The content blocks a sampling message is made of — including [[ToolUseContent]] and
  * [[ToolResultContent]], which the schema declares in this section — are in `Content.scala`,
  * because they share [[TextContent]], [[ImageContent]] and [[AudioContent]] with [[ContentBlock]]
  * and both unions have to be sealed in one file to stay exhaustive.
  */

/** How much surrounding context a server is asking the client to attach (schema.ts:1588-1595).
  *
  * `ThisServer` and `AllServers` are soft-deprecated: a server should only reach for them when the
  * client declared `sampling.context`.
  */
enum IncludeContext(val wire: String) extends Wire:
  case None extends IncludeContext("none")
  case ThisServer extends IncludeContext("thisServer")
  case AllServers extends IncludeContext("allServers")

object IncludeContext:
  val decoder: Decoder[IncludeContext] = Decode.wireEnum(IncludeContext.values)
  val encoder: Encoder[IncludeContext] = Encode.wire

/** Whether the model may, must or must not call a tool (schema.ts:1629-1637). */
enum ToolChoiceMode(val wire: String) extends Wire:
  case Auto extends ToolChoiceMode("auto")
  case Required extends ToolChoiceMode("required")
  case None extends ToolChoiceMode("none")

object ToolChoiceMode:
  val decoder: Decoder[ToolChoiceMode] = Decode.wireEnum(ToolChoiceMode.values)
  val encoder: Encoder[ToolChoiceMode] = Encode.wire

/** schema.ts:1624-1637. Absent `mode` means `Auto`. */
final case class ToolChoice(mode: Option[ToolChoiceMode])

object ToolChoice:
  val decoder: Decoder[ToolChoice] = Decode.obj: fields =>
    fields.opt("mode", ToolChoiceMode.decoder).map(ToolChoice.apply)

  val encoder: Encoder[ToolChoice] = Encode.obj: (choice, out) =>
    out.putOpt("mode", choice.mode, ToolChoiceMode.encoder)

/** A substring of a model name the server would prefer (schema.ts:1971-1992). */
final case class ModelHint(name: Option[String])

object ModelHint:
  val decoder: Decoder[ModelHint] = Decode.obj: fields =>
    fields.opt("name", Decode.string).map(ModelHint.apply)

  val encoder: Encoder[ModelHint] = Encode.obj: (hint, out) =>
    out.putOpt("name", hint.name, Encode.string)

/** What the server would like out of the model it is borrowing (schema.ts:1910-1969).
  *
  * Advisory, all of it. The three priorities run 0 to 1; as with [[Annotations]], the bounds are
  * not enforced at the boundary.
  */
final case class ModelPreferences(
  hints: Option[Vector[ModelHint]],
  costPriority: Option[Double],
  speedPriority: Option[Double],
  intelligencePriority: Option[Double]
)

object ModelPreferences:
  val decoder: Decoder[ModelPreferences] = Decode.obj: fields =>
    for
      hints <- fields.opt("hints", Decode.vector(ModelHint.decoder))
      costPriority <- fields.opt("costPriority", Decode.number)
      speedPriority <- fields.opt("speedPriority", Decode.number)
      intelligencePriority <- fields.opt("intelligencePriority", Decode.number)
    yield ModelPreferences(hints, costPriority, speedPriority, intelligencePriority)

  val encoder: Encoder[ModelPreferences] = Encode.obj: (preferences, out) =>
    out.putOpt("hints", preferences.hints, Encode.vector(ModelHint.encoder))
    out.putOpt("costPriority", preferences.costPriority, Encode.number)
    out.putOpt("speedPriority", preferences.speedPriority, Encode.number)
    out.putOpt("intelligencePriority", preferences.intelligencePriority, Encode.number)

/** Why the model stopped (schema.ts:1662-1673).
  *
  * An open union: the schema lists four values and then admits any string, so this is a `String`
  * with names for the four rather than an enum. A provider-specific reason must survive being read
  * and written back.
  */
object StopReason:
  val EndTurn: String = "endTurn"
  val StopSequence: String = "stopSequence"
  val MaxTokens: String = "maxTokens"
  val ToolUse: String = "toolUse"

/** One turn of a sampling conversation (schema.ts:1676-1688).
  *
  * On the wire `content` is a single block *or* an array of them. It is a `Vector` here, a single
  * block decodes as a one-element vector, and encoding always writes the array form — which the
  * schema allows for every value. That normalisation is the one place in this package where
  * decoding, encoding and decoding again does not reproduce the original bytes: a message that
  * arrived with a bare object leaves with a one-element array. It reproduces the original *value*,
  * which is the property the round-trip tests assert.
  */
final case class SamplingMessage(
  role: Role,
  content: Vector[SamplingMessageContentBlock],
  meta: Option[Json.Obj]
)

object SamplingMessage:
  /** Accepts both wire forms. See the note on [[SamplingMessage]]. */
  private[mcp] val contentDecoder: Decoder[Vector[SamplingMessageContentBlock]] = (at, value) =>
    if js.Array.isArray(value) then
      Decode.vector(SamplingMessageContentBlock.decoder).decode(at, value)
    else SamplingMessageContentBlock.decoder.decode(at, value).map(Vector(_))

  private[mcp] val contentEncoder: Encoder[Vector[SamplingMessageContentBlock]] =
    Encode.vector(SamplingMessageContentBlock.encoder)

  val decoder: Decoder[SamplingMessage] = Decode.obj: fields =>
    for
      role <- fields.req("role", Role.decoder)
      content <- fields.req("content", contentDecoder)
      meta <- fields.opt("_meta", Decode.jsonObj)
    yield SamplingMessage(role, content, meta)

  val encoder: Encoder[SamplingMessage] = Encode.obj: (message, out) =>
    out.put("role", message.role, Role.encoder)
    out.put("content", message.content, contentEncoder)
    out.putOpt("_meta", message.meta, Encode.jsonObj)

/** The params of a `sampling/createMessage` request (schema.ts:1572-1622). */
final case class CreateMessageRequestParams(
  meta: Option[Json.Obj],
  task: Option[TaskMetadata],
  messages: Vector[SamplingMessage],
  modelPreferences: Option[ModelPreferences],
  systemPrompt: Option[String],
  includeContext: Option[IncludeContext],
  temperature: Option[Double],
  maxTokens: Double,
  stopSequences: Option[Vector[String]],
  metadata: Option[Json.Obj],
  tools: Option[Vector[Tool]],
  toolChoice: Option[ToolChoice]
)

object CreateMessageRequestParams:
  val decoder: Decoder[CreateMessageRequestParams] = Decode.obj: fields =>
    for
      meta <- fields.opt("_meta", Decode.jsonObj)
      task <- fields.opt("task", TaskMetadata.decoder)
      messages <- fields.req("messages", Decode.vector(SamplingMessage.decoder))
      modelPreferences <- fields.opt("modelPreferences", ModelPreferences.decoder)
      systemPrompt <- fields.opt("systemPrompt", Decode.string)
      includeContext <- fields.opt("includeContext", IncludeContext.decoder)
      temperature <- fields.opt("temperature", Decode.number)
      maxTokens <- fields.req("maxTokens", Decode.number)
      stopSequences <- fields.opt("stopSequences", Decode.vector(Decode.string))
      metadata <- fields.opt("metadata", Decode.jsonObj)
      tools <- fields.opt("tools", Decode.vector(Tool.decoder))
      toolChoice <- fields.opt("toolChoice", ToolChoice.decoder)
    yield CreateMessageRequestParams(
      meta,
      task,
      messages,
      modelPreferences,
      systemPrompt,
      includeContext,
      temperature,
      maxTokens,
      stopSequences,
      metadata,
      tools,
      toolChoice
    )

  val encoder: Encoder[CreateMessageRequestParams] = Encode.obj: (params, out) =>
    out.putOpt("_meta", params.meta, Encode.jsonObj)
    out.putOpt("task", params.task, TaskMetadata.encoder)
    out.put("messages", params.messages, Encode.vector(SamplingMessage.encoder))
    out.putOpt("modelPreferences", params.modelPreferences, ModelPreferences.encoder)
    out.putOpt("systemPrompt", params.systemPrompt, Encode.string)
    out.putOpt("includeContext", params.includeContext, IncludeContext.encoder)
    out.putOpt("temperature", params.temperature, Encode.number)
    out.put("maxTokens", params.maxTokens, Encode.number)
    out.putOpt("stopSequences", params.stopSequences, Encode.vector(Encode.string))
    out.putOpt("metadata", params.metadata, Encode.jsonObj)
    out.putOpt("tools", params.tools, Encode.vector(Tool.encoder))
    out.putOpt("toolChoice", params.toolChoice, ToolChoice.encoder)

/** What the client's model said (schema.ts:1649-1674).
  *
  * The schema builds this out of both `Result` and `SamplingMessage`, which each contribute a
  * `_meta`; they are the same field, so there is one here.
  */
final case class CreateMessageResult(
  meta: Option[Json.Obj],
  role: Role,
  content: Vector[SamplingMessageContentBlock],
  model: String,
  stopReason: Option[String]
)

object CreateMessageResult:
  val decoder: Decoder[CreateMessageResult] = Decode.obj: fields =>
    for
      meta <- fields.opt("_meta", Decode.jsonObj)
      role <- fields.req("role", Role.decoder)
      content <- fields.req("content", SamplingMessage.contentDecoder)
      model <- fields.req("model", Decode.string)
      stopReason <- fields.opt("stopReason", Decode.string)
    yield CreateMessageResult(meta, role, content, model, stopReason)

  val encoder: Encoder[CreateMessageResult] = Encode.obj: (result, out) =>
    out.putOpt("_meta", result.meta, Encode.jsonObj)
    out.put("role", result.role, Role.encoder)
    out.put("content", result.content, SamplingMessage.contentEncoder)
    out.put("model", result.model, Encode.string)
    out.putOpt("stopReason", result.stopReason, Encode.string)
