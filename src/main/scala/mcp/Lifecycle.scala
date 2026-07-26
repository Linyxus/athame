package mcp

/** `initialize`, `notifications/initialized`, and the capabilities they negotiate
  * (schema.ts:249-457).
  *
  * The capability tree is modelled in full, including the sub-objects of families this package does
  * not otherwise touch — roots, prompts, resources, completions. A client that cannot read a
  * server's `resources.subscribe` cannot decide whether to talk to it at all, so these are data
  * regardless of whether the operations behind them are in scope.
  *
  * The schema types most capability leaves as bare `object`: presence is the whole signal and the
  * contents are unspecified. They are [[Json.Obj]] here, so `Some(Json.Obj(Vector.empty))` is a
  * declared capability with nothing inside and `None` is one that was not declared.
  */

/** schema.ts:316-323. */
final case class RootsCapability(listChanged: Option[Boolean])

object RootsCapability:
  val decoder: Decoder[RootsCapability] = Decode.obj: fields =>
    fields.opt("listChanged", Decode.boolean).map(RootsCapability.apply)

  val encoder: Encoder[RootsCapability] = Encode.obj: (capability, out) =>
    out.putOpt("listChanged", capability.listChanged, Encode.boolean)

/** schema.ts:324-337. */
final case class SamplingCapability(context: Option[Json.Obj], tools: Option[Json.Obj])

object SamplingCapability:
  val decoder: Decoder[SamplingCapability] = Decode.obj: fields =>
    for
      context <- fields.opt("context", Decode.jsonObj)
      tools <- fields.opt("tools", Decode.jsonObj)
    yield SamplingCapability(context, tools)

  val encoder: Encoder[SamplingCapability] = Encode.obj: (capability, out) =>
    out.putOpt("context", capability.context, Encode.jsonObj)
    out.putOpt("tools", capability.tools, Encode.jsonObj)

/** schema.ts:338-341. */
final case class ElicitationCapability(form: Option[Json.Obj], url: Option[Json.Obj])

object ElicitationCapability:
  val decoder: Decoder[ElicitationCapability] = Decode.obj: fields =>
    for
      form <- fields.opt("form", Decode.jsonObj)
      url <- fields.opt("url", Decode.jsonObj)
    yield ElicitationCapability(form, url)

  val encoder: Encoder[ElicitationCapability] = Encode.obj: (capability, out) =>
    out.putOpt("form", capability.form, Encode.jsonObj)
    out.putOpt("url", capability.url, Encode.jsonObj)

/** schema.ts:360-368. */
final case class SamplingTaskSupport(createMessage: Option[Json.Obj])

object SamplingTaskSupport:
  val decoder: Decoder[SamplingTaskSupport] = Decode.obj: fields =>
    fields.opt("createMessage", Decode.jsonObj).map(SamplingTaskSupport.apply)

  val encoder: Encoder[SamplingTaskSupport] = Encode.obj: (support, out) =>
    out.putOpt("createMessage", support.createMessage, Encode.jsonObj)

/** schema.ts:369-376. */
final case class ElicitationTaskSupport(create: Option[Json.Obj])

object ElicitationTaskSupport:
  val decoder: Decoder[ElicitationTaskSupport] = Decode.obj: fields =>
    fields.opt("create", Decode.jsonObj).map(ElicitationTaskSupport.apply)

  val encoder: Encoder[ElicitationTaskSupport] = Encode.obj: (support, out) =>
    out.putOpt("create", support.create, Encode.jsonObj)

/** schema.ts:356-377. */
final case class ClientTaskRequests(
  sampling: Option[SamplingTaskSupport],
  elicitation: Option[ElicitationTaskSupport]
)

object ClientTaskRequests:
  val decoder: Decoder[ClientTaskRequests] = Decode.obj: fields =>
    for
      sampling <- fields.opt("sampling", SamplingTaskSupport.decoder)
      elicitation <- fields.opt("elicitation", ElicitationTaskSupport.decoder)
    yield ClientTaskRequests(sampling, elicitation)

  val encoder: Encoder[ClientTaskRequests] = Encode.obj: (requests, out) =>
    out.putOpt("sampling", requests.sampling, SamplingTaskSupport.encoder)
    out.putOpt("elicitation", requests.elicitation, ElicitationTaskSupport.encoder)

/** schema.ts:342-378. */
final case class ClientTasksCapability(
  list: Option[Json.Obj],
  cancel: Option[Json.Obj],
  requests: Option[ClientTaskRequests]
)

object ClientTasksCapability:
  val decoder: Decoder[ClientTasksCapability] = Decode.obj: fields =>
    for
      list <- fields.opt("list", Decode.jsonObj)
      cancel <- fields.opt("cancel", Decode.jsonObj)
      requests <- fields.opt("requests", ClientTaskRequests.decoder)
    yield ClientTasksCapability(list, cancel, requests)

  val encoder: Encoder[ClientTasksCapability] = Encode.obj: (capability, out) =>
    out.putOpt("list", capability.list, Encode.jsonObj)
    out.putOpt("cancel", capability.cancel, Encode.jsonObj)
    out.putOpt("requests", capability.requests, ClientTaskRequests.encoder)

/** What a client can do (schema.ts:305-379). */
final case class ClientCapabilities(
  experimental: Option[Json.Obj],
  roots: Option[RootsCapability],
  sampling: Option[SamplingCapability],
  elicitation: Option[ElicitationCapability],
  tasks: Option[ClientTasksCapability]
)

object ClientCapabilities:
  /** A client that declares nothing. */
  val empty: ClientCapabilities = ClientCapabilities(None, None, None, None, None)

  val decoder: Decoder[ClientCapabilities] = Decode.obj: fields =>
    for
      experimental <- fields.opt("experimental", Decode.jsonObj)
      roots <- fields.opt("roots", RootsCapability.decoder)
      sampling <- fields.opt("sampling", SamplingCapability.decoder)
      elicitation <- fields.opt("elicitation", ElicitationCapability.decoder)
      tasks <- fields.opt("tasks", ClientTasksCapability.decoder)
    yield ClientCapabilities(experimental, roots, sampling, elicitation, tasks)

  val encoder: Encoder[ClientCapabilities] = Encode.obj: (capabilities, out) =>
    out.putOpt("experimental", capabilities.experimental, Encode.jsonObj)
    out.putOpt("roots", capabilities.roots, RootsCapability.encoder)
    out.putOpt("sampling", capabilities.sampling, SamplingCapability.encoder)
    out.putOpt("elicitation", capabilities.elicitation, ElicitationCapability.encoder)
    out.putOpt("tasks", capabilities.tasks, ClientTasksCapability.encoder)

/** schema.ts:399-407. */
final case class PromptsCapability(listChanged: Option[Boolean])

object PromptsCapability:
  val decoder: Decoder[PromptsCapability] = Decode.obj: fields =>
    fields.opt("listChanged", Decode.boolean).map(PromptsCapability.apply)

  val encoder: Encoder[PromptsCapability] = Encode.obj: (capability, out) =>
    out.putOpt("listChanged", capability.listChanged, Encode.boolean)

/** schema.ts:408-420. */
final case class ResourcesCapability(subscribe: Option[Boolean], listChanged: Option[Boolean])

object ResourcesCapability:
  val decoder: Decoder[ResourcesCapability] = Decode.obj: fields =>
    for
      subscribe <- fields.opt("subscribe", Decode.boolean)
      listChanged <- fields.opt("listChanged", Decode.boolean)
    yield ResourcesCapability(subscribe, listChanged)

  val encoder: Encoder[ResourcesCapability] = Encode.obj: (capability, out) =>
    out.putOpt("subscribe", capability.subscribe, Encode.boolean)
    out.putOpt("listChanged", capability.listChanged, Encode.boolean)

/** schema.ts:421-429. */
final case class ToolsCapability(listChanged: Option[Boolean])

object ToolsCapability:
  val decoder: Decoder[ToolsCapability] = Decode.obj: fields =>
    fields.opt("listChanged", Decode.boolean).map(ToolsCapability.apply)

  val encoder: Encoder[ToolsCapability] = Encode.obj: (capability, out) =>
    out.putOpt("listChanged", capability.listChanged, Encode.boolean)

/** schema.ts:447-454. */
final case class ToolsTaskSupport(call: Option[Json.Obj])

object ToolsTaskSupport:
  val decoder: Decoder[ToolsTaskSupport] = Decode.obj: fields =>
    fields.opt("call", Decode.jsonObj).map(ToolsTaskSupport.apply)

  val encoder: Encoder[ToolsTaskSupport] = Encode.obj: (support, out) =>
    out.putOpt("call", support.call, Encode.jsonObj)

/** schema.ts:443-455. */
final case class ServerTaskRequests(tools: Option[ToolsTaskSupport])

object ServerTaskRequests:
  val decoder: Decoder[ServerTaskRequests] = Decode.obj: fields =>
    fields.opt("tools", ToolsTaskSupport.decoder).map(ServerTaskRequests.apply)

  val encoder: Encoder[ServerTaskRequests] = Encode.obj: (requests, out) =>
    out.putOpt("tools", requests.tools, ToolsTaskSupport.encoder)

/** schema.ts:430-456. */
final case class ServerTasksCapability(
  list: Option[Json.Obj],
  cancel: Option[Json.Obj],
  requests: Option[ServerTaskRequests]
)

object ServerTasksCapability:
  val decoder: Decoder[ServerTasksCapability] = Decode.obj: fields =>
    for
      list <- fields.opt("list", Decode.jsonObj)
      cancel <- fields.opt("cancel", Decode.jsonObj)
      requests <- fields.opt("requests", ServerTaskRequests.decoder)
    yield ServerTasksCapability(list, cancel, requests)

  val encoder: Encoder[ServerTasksCapability] = Encode.obj: (capability, out) =>
    out.putOpt("list", capability.list, Encode.jsonObj)
    out.putOpt("cancel", capability.cancel, Encode.jsonObj)
    out.putOpt("requests", capability.requests, ServerTaskRequests.encoder)

/** What a server offers (schema.ts:381-457). */
final case class ServerCapabilities(
  experimental: Option[Json.Obj],
  logging: Option[Json.Obj],
  completions: Option[Json.Obj],
  prompts: Option[PromptsCapability],
  resources: Option[ResourcesCapability],
  tools: Option[ToolsCapability],
  tasks: Option[ServerTasksCapability]
)

object ServerCapabilities:
  /** A server that offers nothing. */
  val empty: ServerCapabilities = ServerCapabilities(None, None, None, None, None, None, None)

  val decoder: Decoder[ServerCapabilities] = Decode.obj: fields =>
    for
      experimental <- fields.opt("experimental", Decode.jsonObj)
      logging <- fields.opt("logging", Decode.jsonObj)
      completions <- fields.opt("completions", Decode.jsonObj)
      prompts <- fields.opt("prompts", PromptsCapability.decoder)
      resources <- fields.opt("resources", ResourcesCapability.decoder)
      tools <- fields.opt("tools", ToolsCapability.decoder)
      tasks <- fields.opt("tasks", ServerTasksCapability.decoder)
    yield ServerCapabilities(experimental, logging, completions, prompts, resources, tools, tasks)

  val encoder: Encoder[ServerCapabilities] = Encode.obj: (capabilities, out) =>
    out.putOpt("experimental", capabilities.experimental, Encode.jsonObj)
    out.putOpt("logging", capabilities.logging, Encode.jsonObj)
    out.putOpt("completions", capabilities.completions, Encode.jsonObj)
    out.putOpt("prompts", capabilities.prompts, PromptsCapability.encoder)
    out.putOpt("resources", capabilities.resources, ResourcesCapability.encoder)
    out.putOpt("tools", capabilities.tools, ToolsCapability.encoder)
    out.putOpt("tasks", capabilities.tasks, ServerTasksCapability.encoder)

/** The params of an `initialize` request (schema.ts:249-262).
  *
  * `protocolVersion` is the *latest* revision the client supports, not the only one — the server
  * answers with the revision it intends to use, which may be older.
  */
final case class InitializeRequestParams(
  meta: Option[Json.Obj],
  protocolVersion: String,
  capabilities: ClientCapabilities,
  clientInfo: Implementation
)

object InitializeRequestParams:
  val decoder: Decoder[InitializeRequestParams] = Decode.obj: fields =>
    for
      meta <- fields.opt("_meta", Decode.jsonObj)
      protocolVersion <- fields.req("protocolVersion", Decode.string)
      capabilities <- fields.req("capabilities", ClientCapabilities.decoder)
      clientInfo <- fields.req("clientInfo", Implementation.decoder)
    yield InitializeRequestParams(meta, protocolVersion, capabilities, clientInfo)

  val encoder: Encoder[InitializeRequestParams] = Encode.obj: (params, out) =>
    out.putOpt("_meta", params.meta, Encode.jsonObj)
    out.put("protocolVersion", params.protocolVersion, Encode.string)
    out.put("capabilities", params.capabilities, ClientCapabilities.encoder)
    out.put("clientInfo", params.clientInfo, Implementation.encoder)

/** The server's answer to `initialize` (schema.ts:274-293). */
final case class InitializeResult(
  meta: Option[Json.Obj],
  protocolVersion: String,
  capabilities: ServerCapabilities,
  serverInfo: Implementation,
  instructions: Option[String]
)

object InitializeResult:
  val decoder: Decoder[InitializeResult] = Decode.obj: fields =>
    for
      meta <- fields.opt("_meta", Decode.jsonObj)
      protocolVersion <- fields.req("protocolVersion", Decode.string)
      capabilities <- fields.req("capabilities", ServerCapabilities.decoder)
      serverInfo <- fields.req("serverInfo", Implementation.decoder)
      instructions <- fields.opt("instructions", Decode.string)
    yield InitializeResult(meta, protocolVersion, capabilities, serverInfo, instructions)

  val encoder: Encoder[InitializeResult] = Encode.obj: (result, out) =>
    out.putOpt("_meta", result.meta, Encode.jsonObj)
    out.put("protocolVersion", result.protocolVersion, Encode.string)
    out.put("capabilities", result.capabilities, ServerCapabilities.encoder)
    out.put("serverInfo", result.serverInfo, Implementation.encoder)
    out.putOpt("instructions", result.instructions, Encode.string)
