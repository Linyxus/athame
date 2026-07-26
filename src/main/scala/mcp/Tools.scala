package mcp

/** Tool definitions, listing and invocation (schema.ts:1080-1297).
  *
  * `tools/list` and `tools/call` are requests, so their envelopes live in [[ClientRequest]]; what
  * is here is the params and results they carry, plus [[Tool]] itself.
  */

/** The JSON Schema that describes a tool's arguments or its structured output
  * (schema.ts:1257-1284).
  *
  * The schema pins `type` to the literal `"object"`, so the field is not modelled — it is checked
  * on the way in and written on the way out. `properties` is arbitrary JSON: this package
  * transports schemas, it does not interpret them, and a JSON Schema draft we have never heard of
  * must survive the trip intact.
  */
final case class JsonSchemaObject(
  schema: Option[String],
  properties: Option[Json.Obj],
  required: Option[Vector[String]]
)

object JsonSchemaObject:
  /** The empty `{"type":"object"}` — what a tool taking no arguments declares. */
  val empty: JsonSchemaObject = JsonSchemaObject(None, None, None)

  val decoder: Decoder[JsonSchemaObject] = Decode.obj: fields =>
    for
      schema <- fields.opt("$schema", Decode.string)
      _ <- fields.req("type", Decode.literal("object"))
      properties <- fields.opt("properties", Decode.jsonObj)
      required <- fields.opt("required", Decode.vector(Decode.string))
    yield JsonSchemaObject(schema, properties, required)

  val encoder: Encoder[JsonSchemaObject] = Encode.obj: (schema, out) =>
    out.putOpt("$schema", schema.schema, Encode.string)
    out.put("type", "object", Encode.string)
    out.putOpt("properties", schema.properties, Encode.jsonObj)
    out.putOpt("required", schema.required, Encode.vector(Encode.string))

/** Hints about what calling a tool will do (schema.ts:1168-1222).
  *
  * Every field is a hint from whoever wrote the tool, and none of them is a guarantee — the schema
  * says so twice. Decoding one changes nothing about how much you should trust it.
  */
final case class ToolAnnotations(
  title: Option[String],
  readOnlyHint: Option[Boolean],
  destructiveHint: Option[Boolean],
  idempotentHint: Option[Boolean],
  openWorldHint: Option[Boolean]
)

object ToolAnnotations:
  val decoder: Decoder[ToolAnnotations] = Decode.obj: fields =>
    for
      title <- fields.opt("title", Decode.string)
      readOnlyHint <- fields.opt("readOnlyHint", Decode.boolean)
      destructiveHint <- fields.opt("destructiveHint", Decode.boolean)
      idempotentHint <- fields.opt("idempotentHint", Decode.boolean)
      openWorldHint <- fields.opt("openWorldHint", Decode.boolean)
    yield ToolAnnotations(title, readOnlyHint, destructiveHint, idempotentHint, openWorldHint)

  val encoder: Encoder[ToolAnnotations] = Encode.obj: (annotations, out) =>
    out.putOpt("title", annotations.title, Encode.string)
    out.putOpt("readOnlyHint", annotations.readOnlyHint, Encode.boolean)
    out.putOpt("destructiveHint", annotations.destructiveHint, Encode.boolean)
    out.putOpt("idempotentHint", annotations.idempotentHint, Encode.boolean)
    out.putOpt("openWorldHint", annotations.openWorldHint, Encode.boolean)

/** Whether a tool can, may or must be run as a task (schema.ts:1234-1241). Absent means
  * `Forbidden`.
  */
enum TaskSupport(val wire: String) extends Wire:
  case Forbidden extends TaskSupport("forbidden")
  case Optional extends TaskSupport("optional")
  case Required extends TaskSupport("required")

object TaskSupport:
  val decoder: Decoder[TaskSupport] = Decode.wireEnum(TaskSupport.values)
  val encoder: Encoder[TaskSupport] = Encode.wire

/** schema.ts:1224-1242. */
final case class ToolExecution(taskSupport: Option[TaskSupport])

object ToolExecution:
  val decoder: Decoder[ToolExecution] = Decode.obj: fields =>
    fields.opt("taskSupport", TaskSupport.decoder).map(ToolExecution.apply)

  val encoder: Encoder[ToolExecution] = Encode.obj: (execution, out) =>
    out.putOpt("taskSupport", execution.taskSupport, TaskSupport.encoder)

/** A tool a server offers (schema.ts:1244-1297). */
final case class Tool(
  name: String,
  title: Option[String],
  icons: Option[Vector[Icon]],
  description: Option[String],
  inputSchema: JsonSchemaObject,
  execution: Option[ToolExecution],
  outputSchema: Option[JsonSchemaObject],
  annotations: Option[ToolAnnotations],
  meta: Option[Json.Obj]
) extends BaseMetadata,
      Icons

object Tool:
  val decoder: Decoder[Tool] = Decode.obj: fields =>
    for
      name <- fields.req("name", Decode.string)
      title <- fields.opt("title", Decode.string)
      icons <- fields.opt("icons", Decode.vector(Icon.decoder))
      description <- fields.opt("description", Decode.string)
      inputSchema <- fields.req("inputSchema", JsonSchemaObject.decoder)
      execution <- fields.opt("execution", ToolExecution.decoder)
      outputSchema <- fields.opt("outputSchema", JsonSchemaObject.decoder)
      annotations <- fields.opt("annotations", ToolAnnotations.decoder)
      meta <- fields.opt("_meta", Decode.jsonObj)
    yield Tool(name, title, icons, description, inputSchema, execution, outputSchema, annotations, meta)

  val encoder: Encoder[Tool] = Encode.obj: (tool, out) =>
    out.put("name", tool.name, Encode.string)
    out.putOpt("title", tool.title, Encode.string)
    out.putOpt("icons", tool.icons, Encode.vector(Icon.encoder))
    out.putOpt("description", tool.description, Encode.string)
    out.put("inputSchema", tool.inputSchema, JsonSchemaObject.encoder)
    out.putOpt("execution", tool.execution, ToolExecution.encoder)
    out.putOpt("outputSchema", tool.outputSchema, JsonSchemaObject.encoder)
    out.putOpt("annotations", tool.annotations, ToolAnnotations.encoder)
    out.putOpt("_meta", tool.meta, Encode.jsonObj)

/** The answer to `tools/list` (schema.ts:1090-1097). */
final case class ListToolsResult(
  meta: Option[Json.Obj],
  nextCursor: Option[Cursor],
  tools: Vector[Tool]
)

object ListToolsResult:
  val decoder: Decoder[ListToolsResult] = Decode.obj: fields =>
    for
      meta <- fields.opt("_meta", Decode.jsonObj)
      nextCursor <- fields.opt("nextCursor", Decode.string)
      tools <- fields.req("tools", Decode.vector(Tool.decoder))
    yield ListToolsResult(meta, nextCursor, tools)

  val encoder: Encoder[ListToolsResult] = Encode.obj: (result, out) =>
    out.putOpt("_meta", result.meta, Encode.jsonObj)
    out.putOpt("nextCursor", result.nextCursor, Encode.string)
    out.put("tools", result.tools, Encode.vector(Tool.encoder))

/** The params of a `tools/call` request (schema.ts:1132-1146). */
final case class CallToolRequestParams(
  meta: Option[Json.Obj],
  task: Option[TaskMetadata],
  name: String,
  arguments: Option[Json.Obj]
)

object CallToolRequestParams:
  val decoder: Decoder[CallToolRequestParams] = Decode.obj: fields =>
    for
      meta <- fields.opt("_meta", Decode.jsonObj)
      task <- fields.opt("task", TaskMetadata.decoder)
      name <- fields.req("name", Decode.string)
      arguments <- fields.opt("arguments", Decode.jsonObj)
    yield CallToolRequestParams(meta, task, name, arguments)

  val encoder: Encoder[CallToolRequestParams] = Encode.obj: (params, out) =>
    out.putOpt("_meta", params.meta, Encode.jsonObj)
    out.putOpt("task", params.task, TaskMetadata.encoder)
    out.put("name", params.name, Encode.string)
    out.putOpt("arguments", params.arguments, Encode.jsonObj)

/** What a tool call produced (schema.ts:1099-1130).
  *
  * `isError` reports a failure *of the tool*, which is a result and not a protocol error: the model
  * has to see it to correct itself. A tool that could not be found is the other thing, and that one
  * is a JSON-RPC error response.
  */
final case class CallToolResult(
  meta: Option[Json.Obj],
  content: Vector[ContentBlock],
  structuredContent: Option[Json.Obj],
  isError: Option[Boolean]
)

object CallToolResult:
  val decoder: Decoder[CallToolResult] = Decode.obj: fields =>
    for
      meta <- fields.opt("_meta", Decode.jsonObj)
      content <- fields.req("content", Decode.vector(ContentBlock.decoder))
      structuredContent <- fields.opt("structuredContent", Decode.jsonObj)
      isError <- fields.opt("isError", Decode.boolean)
    yield CallToolResult(meta, content, structuredContent, isError)

  val encoder: Encoder[CallToolResult] = Encode.obj: (result, out) =>
    out.putOpt("_meta", result.meta, Encode.jsonObj)
    out.put("content", result.content, Encode.vector(ContentBlock.encoder))
    out.putOpt("structuredContent", result.structuredContent, Encode.jsonObj)
    out.putOpt("isError", result.isError, Encode.boolean)
