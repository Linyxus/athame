package mcp

/** Shared plumbing for the `mcp` suites.
  *
  * Decoding returns an `Either`, and a test that asserts on the wrong side of it should say so
  * rather than pattern-match its way to a confusing failure. [[rendered]] in particular is how every
  * error-path test reads the contract: the rendered string, not the error's structure.
  */
trait McpSuite extends munit.FunSuite:

  /** The value `text` decodes to, or a failure naming the error. */
  def decoded[A](text: String, decoder: Decoder[A])(using munit.Location): A =
    Codec.decode(text, decoder) match
      case Right(value) => value
      case Left(error)  => fail(s"expected $text to decode, got ${error.render}")

  /** The rendered complaint `text` produces, or a failure naming the value it decoded to. */
  def rendered[A](text: String, decoder: Decoder[A])(using munit.Location): String =
    Codec.decode(text, decoder) match
      case Left(error)  => error.render
      case Right(value) => fail(s"expected $text to be refused, got $value")

  /** One pinned pair, asserted in both directions.
    *
    * A golden vector is a claim about bytes *and* about the value they mean, and the two halves
    * hold each other honest: an encoder that reorders fields fails the second assertion, a decoder
    * that drops one fails the first.
    */
  def assertGolden[A](json: String, value: A, decoder: Decoder[A], encoder: Encoder[A])(using
    munit.Location
  ): Unit =
    assertEquals(decoded(json, decoder), value)
    assertEquals(Codec.encode(value, encoder), json)

  /** decode ∘ encode, the identity the round-trip tests assert. */
  def roundTrip[A](value: A, decoder: Decoder[A], encoder: Encoder[A])(using
    munit.Location
  ): A =
    decoded(Codec.encode(value, encoder), decoder)

  def assertRoundTrips[A](value: A, decoder: Decoder[A], encoder: Encoder[A])(using
    munit.Location
  ): Unit =
    assertEquals(roundTrip(value, decoder, encoder), value)

/** Hand-built values covering every branch of every union in the package.
  *
  * The schema file this package is written against carries no example payloads — it is a type
  * declaration — so these are written from the field definitions rather than lifted from published
  * examples, and the golden suites cite the schema line ranges they were read off instead.
  *
  * Types with meaningful optionality come in two shapes: a `…Minimal` with every optional field
  * absent and a `…Full` with all of them present. Both directions are tested for each, because a
  * codec that confuses "absent" with "present and empty" passes one and fails the other.
  */
object Sample:

  val meta: Json.Obj = Json.obj(
    "io.modelcontextprotocol/related-task" -> Json.obj("taskId" -> Json.Str("t-1")),
    "trace" -> Json.Str("abc")
  )

  val iconLight: Icon = Icon("https://example.test/i.png", Some("image/png"), Some(Vector("48x48")), Some(IconTheme.Light))
  val iconDark: Icon = Icon("data:image/svg+xml;base64,PHN2Zy8+", None, None, Some(IconTheme.Dark))
  val iconBare: Icon = Icon("https://example.test/bare.png", None, None, None)

  val annotationsFull: Annotations =
    Annotations(Some(Vector(Role.User, Role.Assistant)), Some(0.5), Some("2025-01-12T15:00:58Z"))
  val annotationsEmpty: Annotations = Annotations(None, None, None)

  val implementationMinimal: Implementation =
    Implementation("athame", None, None, "1.0.0", None, None)
  val implementationFull: Implementation = Implementation(
    "athame",
    Some("Athame"),
    Some(Vector(iconLight, iconDark)),
    "1.0.0",
    Some("A sync tool"),
    Some("https://example.test/")
  )

  // --- content -------------------------------------------------------------------------------

  val textMinimal: TextContent = TextContent("hello", None, None)
  val textFull: TextContent = TextContent("hello", Some(annotationsFull), Some(meta))
  val image: ImageContent = ImageContent("aGk=", "image/png", Some(annotationsEmpty), None)
  val audio: AudioContent = AudioContent("aGk=", "audio/wav", None, Some(meta))

  val resourceMinimal: Resource =
    Resource("readme", None, None, "file:///readme.md", None, None, None, None, None)
  val resourceFull: Resource = Resource(
    "readme",
    Some("README"),
    Some(Vector(iconBare)),
    "file:///readme.md",
    Some("The readme"),
    Some("text/markdown"),
    Some(annotationsFull),
    Some(1024),
    Some(meta)
  )

  val resourceLink: ResourceLink = ResourceLink(resourceFull)
  val resourceLinkMinimal: ResourceLink = ResourceLink(resourceMinimal)

  val textContents: TextResourceContents =
    TextResourceContents("file:///readme.md", Some("text/markdown"), None, "# readme")
  val blobContents: BlobResourceContents =
    BlobResourceContents("file:///logo.png", Some("image/png"), Some(meta), "aGk=")

  val embeddedText: EmbeddedResource = EmbeddedResource(textContents, None, None)
  val embeddedBlob: EmbeddedResource =
    EmbeddedResource(blobContents, Some(annotationsFull), Some(meta))

  val toolUse: ToolUseContent =
    ToolUseContent("call_1", "search", Json.obj("q" -> Json.Str("scala")), Some(meta))
  val toolResult: ToolResultContent = ToolResultContent(
    "call_1",
    Vector(textMinimal, resourceLink),
    Some(Json.obj("hits" -> Json.Num(3))),
    Some(false),
    None
  )

  /** Every member of [[ContentBlock]]. */
  val contentBlocks: Vector[ContentBlock] =
    Vector(textMinimal, textFull, image, audio, resourceLink, resourceLinkMinimal, embeddedText, embeddedBlob)

  /** Every member of [[SamplingMessageContentBlock]]. */
  val samplingBlocks: Vector[SamplingMessageContentBlock] =
    Vector(textMinimal, textFull, image, audio, toolUse, toolResult)

  // --- tools ---------------------------------------------------------------------------------

  val inputSchema: JsonSchemaObject = JsonSchemaObject(
    Some("https://json-schema.org/draft/2020-12/schema"),
    Some(Json.obj("q" -> Json.obj("type" -> Json.Str("string")))),
    Some(Vector("q"))
  )

  val toolMinimal: Tool =
    Tool("search", None, None, None, JsonSchemaObject.empty, None, None, None, None)
  val toolFull: Tool = Tool(
    "search",
    Some("Search"),
    Some(Vector(iconBare)),
    Some("Searches things"),
    inputSchema,
    Some(ToolExecution(Some(TaskSupport.Optional))),
    Some(JsonSchemaObject(None, Some(Json.obj("hits" -> Json.obj("type" -> Json.Str("number")))), None)),
    Some(ToolAnnotations(Some("Search"), Some(true), Some(false), Some(true), Some(false))),
    Some(meta)
  )

  val listToolsMinimal: ListToolsResult = ListToolsResult(None, None, Vector.empty)
  val listToolsFull: ListToolsResult =
    ListToolsResult(Some(meta), Some("cursor-2"), Vector(toolMinimal, toolFull))

  val callToolMinimal: CallToolRequestParams = CallToolRequestParams(None, None, "search", None)
  val callToolFull: CallToolRequestParams = CallToolRequestParams(
    Some(meta),
    Some(TaskMetadata(Some(60000))),
    "search",
    Some(Json.obj("q" -> Json.Str("scala"), "limit" -> Json.Num(10)))
  )

  val callToolResultMinimal: CallToolResult = CallToolResult(None, Vector.empty, None, None)
  val callToolResultFull: CallToolResult =
    CallToolResult(Some(meta), contentBlocks, Some(Json.obj("hits" -> Json.Num(3))), Some(true))

  // --- lifecycle -----------------------------------------------------------------------------

  val clientCapabilitiesFull: ClientCapabilities = ClientCapabilities(
    Some(Json.obj("x.custom" -> Json.obj())),
    Some(RootsCapability(Some(true))),
    Some(SamplingCapability(Some(Json.obj()), Some(Json.obj()))),
    Some(ElicitationCapability(Some(Json.obj()), Some(Json.obj()))),
    Some(
      ClientTasksCapability(
        Some(Json.obj()),
        Some(Json.obj()),
        Some(
          ClientTaskRequests(
            Some(SamplingTaskSupport(Some(Json.obj()))),
            Some(ElicitationTaskSupport(Some(Json.obj())))
          )
        )
      )
    )
  )

  val serverCapabilitiesFull: ServerCapabilities = ServerCapabilities(
    Some(Json.obj("x.custom" -> Json.obj())),
    Some(Json.obj()),
    Some(Json.obj()),
    Some(PromptsCapability(Some(false))),
    Some(ResourcesCapability(Some(true), Some(true))),
    Some(ToolsCapability(Some(true))),
    Some(
      ServerTasksCapability(
        Some(Json.obj()),
        Some(Json.obj()),
        Some(ServerTaskRequests(Some(ToolsTaskSupport(Some(Json.obj())))))
      )
    )
  )

  val initializeMinimal: InitializeRequestParams =
    InitializeRequestParams(None, Mcp.LatestProtocolVersion, ClientCapabilities.empty, implementationMinimal)
  val initializeFull: InitializeRequestParams =
    InitializeRequestParams(Some(meta), Mcp.LatestProtocolVersion, clientCapabilitiesFull, implementationFull)

  val initializeResultMinimal: InitializeResult =
    InitializeResult(None, Mcp.LatestProtocolVersion, ServerCapabilities.empty, implementationMinimal, None)
  val initializeResultFull: InitializeResult = InitializeResult(
    Some(meta),
    Mcp.LatestProtocolVersion,
    serverCapabilitiesFull,
    implementationFull,
    Some("Use the search tool.")
  )

  // --- sampling ------------------------------------------------------------------------------

  val samplingMessage: SamplingMessage = SamplingMessage(Role.User, Vector(textMinimal), None)
  val samplingMessageFull: SamplingMessage =
    SamplingMessage(Role.Assistant, samplingBlocks, Some(meta))

  val createMessageMinimal: CreateMessageRequestParams = CreateMessageRequestParams(
    None, None, Vector(samplingMessage), None, None, None, None, 1024, None, None, None, None
  )
  val createMessageFull: CreateMessageRequestParams = CreateMessageRequestParams(
    Some(meta),
    Some(TaskMetadata(Some(30000))),
    Vector(samplingMessage, samplingMessageFull),
    Some(ModelPreferences(Some(Vector(ModelHint(Some("sonnet")), ModelHint(None))), Some(0.1), Some(0.2), Some(0.9))),
    Some("Be brief."),
    Some(IncludeContext.ThisServer),
    Some(0.7),
    1024,
    Some(Vector("STOP")),
    Some(Json.obj("provider" -> Json.Str("x"))),
    Some(Vector(toolMinimal)),
    Some(ToolChoice(Some(ToolChoiceMode.Required)))
  )

  val createMessageResultMinimal: CreateMessageResult =
    CreateMessageResult(None, Role.Assistant, Vector(textMinimal), "claude-x", None)
  val createMessageResultFull: CreateMessageResult =
    CreateMessageResult(Some(meta), Role.Assistant, samplingBlocks, "claude-x", Some(StopReason.ToolUse))

  // --- elicitation ---------------------------------------------------------------------------

  val stringSchema: StringSchema =
    StringSchema(Some("Name"), Some("Your name"), Some(1), Some(64), Some(StringFormat.Email), Some("a@b.test"))
  val stringSchemaMinimal: StringSchema = StringSchema(None, None, None, None, None, None)
  val numberSchema: NumberSchema =
    NumberSchema(NumberType.Number, Some("Ratio"), None, Some(0), Some(1), Some(0.5))
  val integerSchema: NumberSchema =
    NumberSchema(NumberType.Integer, None, None, None, None, None)
  val booleanSchema: BooleanSchema = BooleanSchema(Some("Agree"), None, Some(false))
  val untitledSingle: UntitledSingleSelectEnumSchema =
    UntitledSingleSelectEnumSchema(Some("Colour"), None, Vector("red", "green"), Some("red"))
  val titledSingle: TitledSingleSelectEnumSchema = TitledSingleSelectEnumSchema(
    None,
    Some("Pick one"),
    Vector(EnumOption("r", "Red"), EnumOption("g", "Green")),
    None
  )
  val legacyTitled: LegacyTitledEnumSchema =
    LegacyTitledEnumSchema(None, None, Vector("r", "g"), Vector("Red", "Green"), Some("r"))
  val untitledMulti: UntitledMultiSelectEnumSchema = UntitledMultiSelectEnumSchema(
    Some("Tags"),
    None,
    Some(1),
    Some(3),
    UntitledMultiSelectItems(Vector("a", "b")),
    Some(Vector("a"))
  )
  val titledMulti: TitledMultiSelectEnumSchema = TitledMultiSelectEnumSchema(
    None,
    None,
    None,
    None,
    TitledMultiSelectItems(Vector(EnumOption("a", "Alpha"))),
    None
  )

  /** Every member of [[PrimitiveSchemaDefinition]]. */
  val primitiveSchemas: Vector[PrimitiveSchemaDefinition] = Vector(
    stringSchema,
    stringSchemaMinimal,
    numberSchema,
    integerSchema,
    booleanSchema,
    untitledSingle,
    titledSingle,
    legacyTitled,
    untitledMulti,
    titledMulti
  )

  val requestedSchema: RequestedSchema = RequestedSchema(
    Some("https://json-schema.org/draft/2020-12/schema"),
    Vector("name" -> stringSchema, "colour" -> untitledSingle),
    Some(Vector("name"))
  )

  val elicitForm: ElicitRequestFormParams =
    ElicitRequestFormParams(None, None, "Who are you?", requestedSchema)
  val elicitFormFull: ElicitRequestFormParams = ElicitRequestFormParams(
    Some(meta),
    Some(TaskMetadata(Some(1000))),
    "Who are you?",
    RequestedSchema(None, primitiveSchemas.zipWithIndex.map((s, i) => s"f$i" -> s), None)
  )
  val elicitUrl: ElicitRequestUrlParams =
    ElicitRequestUrlParams(None, None, "Sign in", "e-1", "https://example.test/auth")
  val elicitUrlFull: ElicitRequestUrlParams = ElicitRequestUrlParams(
    Some(meta),
    Some(TaskMetadata(None)),
    "Sign in",
    "e-2",
    "https://example.test/auth"
  )

  val elicitResultMinimal: ElicitResult = ElicitResult(None, ElicitAction.Cancel, None)
  val elicitResultFull: ElicitResult = ElicitResult(
    Some(meta),
    ElicitAction.Accept,
    Some(Json.obj("name" -> Json.Str("ada"), "colour" -> Json.Str("red")))
  )

  // --- utility -------------------------------------------------------------------------------

  val cancelledMinimal: CancelledNotificationParams = CancelledNotificationParams(None, None, None)
  val cancelledFull: CancelledNotificationParams =
    CancelledNotificationParams(Some(meta), Some(RequestId.Num(7)), Some("user asked"))
  val progressMinimal: ProgressNotificationParams =
    ProgressNotificationParams(None, ProgressToken.Str("p-1"), 1, None, None)
  val progressFull: ProgressNotificationParams =
    ProgressNotificationParams(Some(meta), ProgressToken.Num(42), 0.5, Some(1), Some("halfway"))

  val loggingMinimal: LoggingMessageNotificationParams =
    LoggingMessageNotificationParams(None, LoggingLevel.Debug, None, Json.Null)
  val loggingFull: LoggingMessageNotificationParams = LoggingMessageNotificationParams(
    Some(meta),
    LoggingLevel.Emergency,
    Some("db"),
    Json.obj("message" -> Json.Str("disk full"))
  )

  // --- direction unions ----------------------------------------------------------------------

  /** Every member of [[ClientRequest]]. */
  val clientRequests: Vector[ClientRequest] = Vector(
    ClientRequest.Ping(None),
    ClientRequest.Ping(Some(RequestParams(Some(meta)))),
    ClientRequest.Initialize(initializeMinimal),
    ClientRequest.Initialize(initializeFull),
    ClientRequest.SetLevel(SetLevelRequestParams(None, LoggingLevel.Warning)),
    ClientRequest.CallTool(callToolMinimal),
    ClientRequest.CallTool(callToolFull),
    ClientRequest.ListTools(None),
    ClientRequest.ListTools(Some(PaginatedRequestParams(Some(meta), Some("c-1"))))
  )

  /** Every member of [[ClientNotification]]. */
  val clientNotifications: Vector[ClientNotification] = Vector(
    ClientNotification.Cancelled(cancelledMinimal),
    ClientNotification.Cancelled(cancelledFull),
    ClientNotification.Progress(progressMinimal),
    ClientNotification.Progress(progressFull),
    ClientNotification.Initialized(None),
    ClientNotification.Initialized(Some(NotificationParams(Some(meta))))
  )

  /** Every member of [[ClientResult]]. */
  val clientResults: Vector[ClientResult] = Vector(
    ClientResult.Empty(EmptyResult.empty),
    ClientResult.Empty(EmptyResult(Some(meta))),
    ClientResult.CreateMessage(createMessageResultMinimal),
    ClientResult.CreateMessage(createMessageResultFull),
    ClientResult.Elicit(elicitResultMinimal),
    ClientResult.Elicit(elicitResultFull)
  )

  /** Every member of [[ServerRequest]]. */
  val serverRequests: Vector[ServerRequest] = Vector(
    ServerRequest.Ping(None),
    ServerRequest.Ping(Some(RequestParams(None))),
    ServerRequest.CreateMessage(createMessageMinimal),
    ServerRequest.CreateMessage(createMessageFull),
    ServerRequest.Elicit(elicitForm),
    ServerRequest.Elicit(elicitFormFull),
    ServerRequest.Elicit(elicitUrl),
    ServerRequest.Elicit(elicitUrlFull)
  )

  /** Every member of [[ServerNotification]]. */
  val serverNotifications: Vector[ServerNotification] = Vector(
    ServerNotification.Cancelled(cancelledMinimal),
    ServerNotification.Progress(progressFull),
    ServerNotification.LoggingMessage(loggingMinimal),
    ServerNotification.LoggingMessage(loggingFull),
    ServerNotification.ToolListChanged(None),
    ServerNotification.ToolListChanged(Some(NotificationParams(Some(meta)))),
    ServerNotification.ElicitationComplete(ElicitationCompleteNotificationParams("e-1"))
  )

  /** Every member of [[ServerResult]]. */
  val serverResults: Vector[ServerResult] = Vector(
    ServerResult.Empty(EmptyResult.empty),
    ServerResult.Initialize(initializeResultMinimal),
    ServerResult.Initialize(initializeResultFull),
    ServerResult.CallTool(callToolResultMinimal),
    ServerResult.CallTool(callToolResultFull),
    ServerResult.ListTools(listToolsMinimal),
    ServerResult.ListTools(listToolsFull)
  )

  val errorMinimal: JsonRpc.Error =
    JsonRpc.Error(ErrorCode.MethodNotFound, "Method not found", None)
  val errorFull: JsonRpc.Error =
    JsonRpc.Error(ErrorCode.InvalidParams, "Invalid params", Some(Json.obj("field" -> Json.Str("name"))))

  /** Every shape of [[ClientMessage]], over both spellings of a request id. */
  val clientMessages: Vector[ClientMessage] =
    clientRequests.map(ClientMessage.Request(RequestId.Num(1), _)) ++
      clientRequests.map(ClientMessage.Request(RequestId.Str("a"), _)) ++
      clientNotifications.map(ClientMessage.Notification.apply) ++
      clientResults.map(ClientMessage.response(RequestId.Num(2), _)) ++
      Vector(
        ClientMessage.Error(Some(RequestId.Num(3)), errorMinimal),
        ClientMessage.Error(Some(RequestId.Str("b")), errorFull),
        ClientMessage.Error(None, errorFull)
      )

  /** Every shape of [[ServerMessage]], over both spellings of a request id. */
  val serverMessages: Vector[ServerMessage] =
    serverRequests.map(ServerMessage.Request(RequestId.Num(1), _)) ++
      serverRequests.map(ServerMessage.Request(RequestId.Str("a"), _)) ++
      serverNotifications.map(ServerMessage.Notification.apply) ++
      serverResults.map(ServerMessage.response(RequestId.Num(2), _)) ++
      Vector(
        ServerMessage.Error(Some(RequestId.Num(3)), errorMinimal),
        ServerMessage.Error(None, errorFull)
      )
