package mcp

/** decode ∘ encode = identity, over hand-built values and over generated ones.
  *
  * The hand-built half walks every branch of every union deliberately, so a member nobody thought
  * about is a compile-time hole in [[Sample]] rather than a gap in coverage. The generated half is
  * a seeded linear congruential generator — the same vectors every run, so a failure is
  * reproducible and the suite never flakes — pushing several thousand values through the same law.
  *
  * The law is about *values*, not bytes. Two places normalise, both documented and both pinned
  * below: a sampling message's single content block becomes a one-element array, and an
  * elicitation form's absent `mode` becomes an explicit `"form"`.
  */
class RoundTripSuite extends McpSuite:

  // --- hand-built, every branch --------------------------------------------------------------

  test("round trip: every content block"):
    for block <- Sample.contentBlocks do
      assertRoundTrips(block, ContentBlock.decoder, ContentBlock.encoder)
    assertEquals(Sample.contentBlocks.map(_.tag).distinct.length, ContentBlock.Tags.length)

  test("round trip: every sampling content block"):
    for block <- Sample.samplingBlocks do
      assertRoundTrips(block, SamplingMessageContentBlock.decoder, SamplingMessageContentBlock.encoder)
    assertEquals(
      Sample.samplingBlocks.map(_.tag).distinct.length,
      SamplingMessageContentBlock.Tags.length
    )

  test("round trip: both kinds of resource contents"):
    for contents <- Vector(Sample.textContents, Sample.blobContents) do
      assertRoundTrips(contents, ResourceContents.decoder, ResourceContents.encoder)

  test("round trip: every primitive schema"):
    assertEquals(Sample.primitiveSchemas.length, 10)
    for schema <- Sample.primitiveSchemas do
      assertRoundTrips(schema, PrimitiveSchemaDefinition.decoder, PrimitiveSchemaDefinition.encoder)

  test("round trip: both elicitation modes"):
    for params <- Vector(Sample.elicitForm, Sample.elicitFormFull, Sample.elicitUrl, Sample.elicitUrlFull) do
      assertRoundTrips(params, ElicitRequestParams.decoder, ElicitRequestParams.encoder)

  test("round trip: both spellings of a request id and a progress token"):
    for id <- Vector(RequestId.Str(""), RequestId.Str("x"), RequestId.Num(0), RequestId.Num(-1.5)) do
      assertRoundTrips(id, RequestId.decoder, RequestId.encoder)
    for token <- Vector(ProgressToken.Str("p"), ProgressToken.Num(1e9)) do
      assertRoundTrips(token, ProgressToken.decoder, ProgressToken.encoder)

  test("round trip: every closed string union, member by member"):
    for role <- Role.values do assertRoundTrips(role, Role.decoder, Role.encoder)
    for theme <- IconTheme.values do assertRoundTrips(theme, IconTheme.decoder, IconTheme.encoder)
    for level <- LoggingLevel.values do assertRoundTrips(level, LoggingLevel.decoder, LoggingLevel.encoder)
    for context <- IncludeContext.values do
      assertRoundTrips(context, IncludeContext.decoder, IncludeContext.encoder)
    for mode <- ToolChoiceMode.values do
      assertRoundTrips(mode, ToolChoiceMode.decoder, ToolChoiceMode.encoder)
    for support <- TaskSupport.values do assertRoundTrips(support, TaskSupport.decoder, TaskSupport.encoder)
    for action <- ElicitAction.values do assertRoundTrips(action, ElicitAction.decoder, ElicitAction.encoder)
    for format <- StringFormat.values do assertRoundTrips(format, StringFormat.decoder, StringFormat.encoder)
    for numberType <- NumberType.values do
      assertRoundTrips(numberType, NumberType.decoder, NumberType.encoder)

  test("round trip: every params and result type, minimal and full"):
    for value <- Vector(Sample.initializeMinimal, Sample.initializeFull) do
      assertRoundTrips(value, InitializeRequestParams.decoder, InitializeRequestParams.encoder)
    for value <- Vector(Sample.initializeResultMinimal, Sample.initializeResultFull) do
      assertRoundTrips(value, InitializeResult.decoder, InitializeResult.encoder)
    for value <- Vector(Sample.toolMinimal, Sample.toolFull) do
      assertRoundTrips(value, Tool.decoder, Tool.encoder)
    for value <- Vector(Sample.listToolsMinimal, Sample.listToolsFull) do
      assertRoundTrips(value, ListToolsResult.decoder, ListToolsResult.encoder)
    for value <- Vector(Sample.callToolMinimal, Sample.callToolFull) do
      assertRoundTrips(value, CallToolRequestParams.decoder, CallToolRequestParams.encoder)
    for value <- Vector(Sample.callToolResultMinimal, Sample.callToolResultFull) do
      assertRoundTrips(value, CallToolResult.decoder, CallToolResult.encoder)
    for value <- Vector(Sample.createMessageMinimal, Sample.createMessageFull) do
      assertRoundTrips(value, CreateMessageRequestParams.decoder, CreateMessageRequestParams.encoder)
    for value <- Vector(Sample.createMessageResultMinimal, Sample.createMessageResultFull) do
      assertRoundTrips(value, CreateMessageResult.decoder, CreateMessageResult.encoder)
    for value <- Vector(Sample.elicitResultMinimal, Sample.elicitResultFull) do
      assertRoundTrips(value, ElicitResult.decoder, ElicitResult.encoder)
    for value <- Vector(Sample.cancelledMinimal, Sample.cancelledFull) do
      assertRoundTrips(value, CancelledNotificationParams.decoder, CancelledNotificationParams.encoder)
    for value <- Vector(Sample.progressMinimal, Sample.progressFull) do
      assertRoundTrips(value, ProgressNotificationParams.decoder, ProgressNotificationParams.encoder)
    for value <- Vector(Sample.loggingMinimal, Sample.loggingFull) do
      assertRoundTrips(
        value,
        LoggingMessageNotificationParams.decoder,
        LoggingMessageNotificationParams.encoder
      )

  test("round trip: every shape of client message"):
    assertEquals(Sample.clientMessages.length, 33)
    for message <- Sample.clientMessages do
      assertEquals(Codec.decodeClientMessage(Codec.encodeClientMessage(message)), Right(message))

  test("round trip: every shape of server message"):
    assertEquals(Sample.serverMessages.length, 32)
    for message <- Sample.serverMessages do
      assertEquals(Codec.decodeServerMessage(Codec.encodeServerMessage(message)), Right(message))

  test("round trip: encoding is deterministic, so the same value is always the same bytes"):
    for message <- Sample.clientMessages do
      assertEquals(Codec.encodeClientMessage(message), Codec.encodeClientMessage(message))

  // --- the two documented normalisations ------------------------------------------------------

  test("normalisation: a single sampling block leaves as an array, and that is the only difference"):
    val single = s"""{"role":"user","content":${Golden.textMinimal}}"""
    val value = decoded(single, SamplingMessage.decoder)
    assertNotEquals(Codec.encode(value, SamplingMessage.encoder), single)
    assertEquals(decoded(Codec.encode(value, SamplingMessage.encoder), SamplingMessage.decoder), value)

  test("normalisation: an absent elicitation mode leaves as form, and that is the only difference"):
    val withoutMode = s"""{"message":"m","requestedSchema":{"type":"object","properties":{}}}"""
    val value = decoded(withoutMode, ElicitRequestParams.decoder)
    assertNotEquals(Codec.encode(value, ElicitRequestParams.encoder), withoutMode)
    assertEquals(
      decoded(Codec.encode(value, ElicitRequestParams.encoder), ElicitRequestParams.decoder),
      value
    )

  test("normalisation: an error's data of null is the same message as no data at all"):
    val explicitNull = JsonRpc.Error(1, "x", Some(Json.Null))
    assertEquals(Codec.encode(explicitNull, JsonRpc.Error.encoder), """{"code":1,"message":"x"}""")
    assertEquals(
      roundTrip(explicitNull, JsonRpc.Error.decoder, JsonRpc.Error.encoder),
      JsonRpc.Error(1, "x", None)
    )

  // --- leniency -------------------------------------------------------------------------------

  test("leniency: unknown fields anywhere are ignored"):
    val padded =
      """{"jsonrpc":"2.0","id":1,"method":"tools/call","x":1,"params":{"name":"t","y":[1],"arguments":{"a":1},"z":{"deep":true}}}"""
    assertEquals(
      Codec.decodeClientMessage(padded),
      Right(
        ClientMessage.Request(
          RequestId.Num(1),
          ClientRequest.CallTool(CallToolRequestParams(None, None, "t", Some(Json.obj("a" -> Json.Num(1)))))
        )
      )
    )

  test("leniency: every optional field spelled null reads as absent"):
    assertEquals(
      decoded(
        """{"name":"search","title":null,"icons":null,"description":null,"inputSchema":{"type":"object","$schema":null,"properties":null,"required":null},"execution":null,"outputSchema":null,"annotations":null,"_meta":null}""",
        Tool.decoder
      ),
      Sample.toolMinimal
    )

  // --- seeded fuzz ----------------------------------------------------------------------------
  //
  // Same seed every run: the vectors are fixed, a failure is reproducible, and nothing here can
  // pass on Tuesday and fail on Wednesday. The generators lean on the awkward parts — empty
  // collections, empty strings, escapes, both members of every union — because that is where a
  // codec breaks.

  private class Rng(seed: Int):
    private var state: Int = seed

    /** The house recurrence, but read from the top.
      *
      * The low bits of a power-of-two LCG have periods as short as the bit position allows: bit 0
      * of this one simply alternates, so a `below(2)` taken from it would make every `option` in a
      * generator flip Some/None in strict turn and the fuzz would explore a fraction of what it
      * looks like it explores. Shifting first costs nothing and the branch-coverage test below is
      * what caught it.
      */
    def next(): Int =
      state = state * 1103515245 + 12345
      state >>> 9

    def below(bound: Int): Int = next() % bound
    def flip(): Boolean = below(2) == 0
    def pick[A](choices: Vector[A]): A = choices(below(choices.length))
    def option[A](generate: => A): Option[A] = if flip() then Some(generate) else None

    def times[A](bound: Int)(generate: => A): Vector[A] =
      Vector.fill(below(bound))(generate)

    /** Eighths in [-250, 250]: finite, exactly representable, and exact through JSON. */
    def number(): Double = (below(4001) - 2000) / 8.0

    def string(): String = times(4)(pick(Rng.fragments)).mkString

    /** Distinct, never array-index-shaped — a key like `"0"` would be hoisted by JavaScript and is
      * tested for separately in JsonSuite rather than smuggled in here.
      */
    def keys(): Vector[String] = times(4)(pick(Rng.keyNames)).distinct

    def json(depth: Int): Json =
      below(if depth <= 0 then 4 else 6) match
        case 0 => Json.Null
        case 1 => Json.Bool(flip())
        case 2 => Json.Num(number())
        case 3 => Json.Str(string())
        case 4 => Json.Arr(times(4)(json(depth - 1)))
        case _ => jsonObj(depth - 1)

    def jsonObj(depth: Int): Json.Obj = Json.Obj(keys().map(key => key -> json(depth)))

    def meta(): Option[Json.Obj] = option(jsonObj(1))

    def annotations(): Annotations =
      Annotations(option(times(3)(pick(Role.values.toVector))), option(number()), option(string()))

    def icon(): Icon =
      Icon(string(), option(string()), option(times(3)(string())), option(pick(IconTheme.values.toVector)))

    def implementation(): Implementation =
      Implementation(
        string(),
        option(string()),
        option(times(3)(icon())),
        string(),
        option(string()),
        option(string())
      )

    def resource(): Resource =
      Resource(
        string(),
        option(string()),
        option(times(2)(icon())),
        string(),
        option(string()),
        option(string()),
        option(annotations()),
        option(number()),
        meta()
      )

    def resourceContents(): ResourceContents =
      if flip() then TextResourceContents(string(), option(string()), meta(), string())
      else BlobResourceContents(string(), option(string()), meta(), string())

    def contentBlock(depth: Int): ContentBlock =
      below(5) match
        case 0 => TextContent(string(), option(annotations()), meta())
        case 1 => ImageContent(string(), string(), option(annotations()), meta())
        case 2 => AudioContent(string(), string(), option(annotations()), meta())
        case 3 => ResourceLink(resource())
        case _ => EmbeddedResource(resourceContents(), option(annotations()), meta())

    def samplingBlock(depth: Int): SamplingMessageContentBlock =
      below(if depth <= 0 then 3 else 5) match
        case 0 => TextContent(string(), option(annotations()), meta())
        case 1 => ImageContent(string(), string(), option(annotations()), meta())
        case 2 => AudioContent(string(), string(), option(annotations()), meta())
        case 3 => ToolUseContent(string(), string(), jsonObj(1), meta())
        case _ =>
          ToolResultContent(
            string(),
            times(3)(contentBlock(depth - 1)),
            option(jsonObj(1)),
            option(flip()),
            meta()
          )

    def schemaObject(): JsonSchemaObject =
      JsonSchemaObject(option(string()), option(jsonObj(1)), option(times(3)(string())))

    def tool(): Tool =
      Tool(
        string(),
        option(string()),
        option(times(2)(icon())),
        option(string()),
        schemaObject(),
        option(ToolExecution(option(pick(TaskSupport.values.toVector)))),
        option(schemaObject()),
        option(
          ToolAnnotations(option(string()), option(flip()), option(flip()), option(flip()), option(flip()))
        ),
        meta()
      )

    def enumOption(): EnumOption = EnumOption(string(), string())

    def primitiveSchema(): PrimitiveSchemaDefinition =
      below(8) match
        case 0 =>
          StringSchema(
            option(string()),
            option(string()),
            option(number()),
            option(number()),
            option(pick(StringFormat.values.toVector)),
            option(string())
          )
        case 1 =>
          NumberSchema(
            pick(NumberType.values.toVector),
            option(string()),
            option(string()),
            option(number()),
            option(number()),
            option(number())
          )
        case 2 => BooleanSchema(option(string()), option(string()), option(flip()))
        case 3 =>
          UntitledSingleSelectEnumSchema(option(string()), option(string()), times(4)(string()), option(string()))
        case 4 =>
          TitledSingleSelectEnumSchema(option(string()), option(string()), times(4)(enumOption()), option(string()))
        case 5 =>
          LegacyTitledEnumSchema(
            option(string()),
            option(string()),
            times(4)(string()),
            times(4)(string()),
            option(string())
          )
        case 6 =>
          UntitledMultiSelectEnumSchema(
            option(string()),
            option(string()),
            option(number()),
            option(number()),
            UntitledMultiSelectItems(times(4)(string())),
            option(times(3)(string()))
          )
        case _ =>
          TitledMultiSelectEnumSchema(
            option(string()),
            option(string()),
            option(number()),
            option(number()),
            TitledMultiSelectItems(times(4)(enumOption())),
            option(times(3)(string()))
          )

    def requestedSchema(): RequestedSchema =
      RequestedSchema(
        option(string()),
        keys().map(key => key -> primitiveSchema()),
        option(times(3)(string()))
      )

    def samplingMessage(): SamplingMessage =
      SamplingMessage(pick(Role.values.toVector), times(3)(samplingBlock(1)), meta())

    def requestId(): RequestId = if flip() then RequestId.Str(string()) else RequestId.Num(number())

    def progressToken(): ProgressToken =
      if flip() then ProgressToken.Str(string()) else ProgressToken.Num(number())

    def taskMetadata(): Option[TaskMetadata] = option(TaskMetadata(option(number())))

    def error(): JsonRpc.Error =
      // Some(Json.Null) is deliberately excluded: it is the same message as None, which the
      // "an error's data of null" test above pins.
      JsonRpc.Error(number(), string(), option(json(2)).filter(_ != Json.Null))

    def clientRequest(): ClientRequest =
      below(5) match
        case 0 => ClientRequest.Ping(option(RequestParams(meta())))
        case 1 =>
          ClientRequest.Initialize(
            InitializeRequestParams(
              meta(),
              string(),
              ClientCapabilities(
                option(jsonObj(1)),
                option(RootsCapability(option(flip()))),
                option(SamplingCapability(option(jsonObj(1)), option(jsonObj(1)))),
                option(ElicitationCapability(option(jsonObj(1)), option(jsonObj(1)))),
                option(ClientTasksCapability(option(jsonObj(1)), option(jsonObj(1)), None))
              ),
              implementation()
            )
          )
        case 2 => ClientRequest.SetLevel(SetLevelRequestParams(meta(), pick(LoggingLevel.values.toVector)))
        case 3 =>
          ClientRequest.CallTool(CallToolRequestParams(meta(), taskMetadata(), string(), option(jsonObj(2))))
        case _ => ClientRequest.ListTools(option(PaginatedRequestParams(meta(), option(string()))))

    def clientNotification(): ClientNotification =
      below(3) match
        case 0 =>
          ClientNotification.Cancelled(
            CancelledNotificationParams(meta(), option(requestId()), option(string()))
          )
        case 1 =>
          ClientNotification.Progress(
            ProgressNotificationParams(meta(), progressToken(), number(), option(number()), option(string()))
          )
        case _ => ClientNotification.Initialized(option(NotificationParams(meta())))

    def clientMessage(): ClientMessage =
      below(4) match
        case 0 => ClientMessage.Request(requestId(), clientRequest())
        case 1 => ClientMessage.Notification(clientNotification())
        case 2 => ClientMessage.Response(requestId(), jsonObj(2))
        case _ => ClientMessage.Error(option(requestId()), error())

    def serverRequest(): ServerRequest =
      below(3) match
        case 0 => ServerRequest.Ping(option(RequestParams(meta())))
        case 1 =>
          ServerRequest.CreateMessage(
            CreateMessageRequestParams(
              meta(),
              taskMetadata(),
              times(3)(samplingMessage()),
              option(
                ModelPreferences(
                  option(times(2)(ModelHint(option(string())))),
                  option(number()),
                  option(number()),
                  option(number())
                )
              ),
              option(string()),
              option(pick(IncludeContext.values.toVector)),
              option(number()),
              number(),
              option(times(3)(string())),
              option(jsonObj(1)),
              option(times(2)(tool())),
              option(ToolChoice(option(pick(ToolChoiceMode.values.toVector))))
            )
          )
        case _ =>
          ServerRequest.Elicit(
            if flip() then ElicitRequestFormParams(meta(), taskMetadata(), string(), requestedSchema())
            else ElicitRequestUrlParams(meta(), taskMetadata(), string(), string(), string())
          )

    def serverNotification(): ServerNotification =
      below(5) match
        case 0 =>
          ServerNotification.Cancelled(
            CancelledNotificationParams(meta(), option(requestId()), option(string()))
          )
        case 1 =>
          ServerNotification.Progress(
            ProgressNotificationParams(meta(), progressToken(), number(), option(number()), option(string()))
          )
        case 2 =>
          ServerNotification.LoggingMessage(
            LoggingMessageNotificationParams(
              meta(),
              pick(LoggingLevel.values.toVector),
              option(string()),
              json(2)
            )
          )
        case 3 => ServerNotification.ToolListChanged(option(NotificationParams(meta())))
        case _ => ServerNotification.ElicitationComplete(ElicitationCompleteNotificationParams(string()))

    def serverMessage(): ServerMessage =
      below(4) match
        case 0 => ServerMessage.Request(requestId(), serverRequest())
        case 1 => ServerMessage.Notification(serverNotification())
        case 2 => ServerMessage.Response(requestId(), jsonObj(2))
        case _ => ServerMessage.Error(option(requestId()), error())

  private object Rng:
    val fragments: Vector[String] =
      Vector("a", "b", "zz", "", "\"", "\\", "\n", "\t", "é", "🜁", "x/y", " ", "0", "null")

    val keyNames: Vector[String] =
      Vector("alpha", "beta", "gamma", "io.example/key", "_private", "a b", "é", "k9", "-1")

  test("fuzz: 2000 Json trees survive toJs and fromJs"):
    val rng = Rng(0x5eed_0101)
    var round = 0
    while round < 2000 do
      val tree = rng.json(3)
      assertEquals(Json.fromJs(Json.toJs(tree)), tree, s"round $round")
      round += 1

  test("fuzz: 2000 Json trees survive stringify and parse"):
    val rng = Rng(0x5eed_0102)
    var round = 0
    while round < 2000 do
      val tree = rng.json(3)
      assertEquals(Json.parse(Json.stringify(tree)), Right(tree), s"round $round")
      round += 1

  test("fuzz: 1500 client messages survive encode and decode"):
    val rng = Rng(0x5eed_0201)
    var round = 0
    while round < 1500 do
      val message = rng.clientMessage()
      val text = Codec.encodeClientMessage(message)
      assertEquals(Codec.decodeClientMessage(text), Right(message), s"round $round: $text")
      round += 1

  test("fuzz: 1500 server messages survive encode and decode"):
    val rng = Rng(0x5eed_0202)
    var round = 0
    while round < 1500 do
      val message = rng.serverMessage()
      val text = Codec.encodeServerMessage(message)
      assertEquals(Codec.decodeServerMessage(text), Right(message), s"round $round: $text")
      round += 1

  test("fuzz: 1000 primitive schemas survive the discrimination rule"):
    // The rule has to recover the member it was given, over every shape the generator can build.
    val rng = Rng(0x5eed_0301)
    var round = 0
    while round < 1000 do
      val schema = rng.primitiveSchema()
      val text = Codec.encode(schema, PrimitiveSchemaDefinition.encoder)
      assertEquals(
        Codec.decode(text, PrimitiveSchemaDefinition.decoder),
        Right(schema),
        s"round $round: $text"
      )
      round += 1

  test("fuzz: 1000 content blocks survive both unions they belong to"):
    val rng = Rng(0x5eed_0401)
    var round = 0
    while round < 1000 do
      val block = rng.contentBlock(2)
      assertRoundTrips(block, ContentBlock.decoder, ContentBlock.encoder)
      val sampling = rng.samplingBlock(2)
      assertRoundTrips(sampling, SamplingMessageContentBlock.decoder, SamplingMessageContentBlock.encoder)
      round += 1

  /** Which member of the schema union a value is, without leaning on reflection. */
  private def label(schema: PrimitiveSchemaDefinition): String =
    schema match
      case _: StringSchema                   => "string"
      case _: NumberSchema                   => "number"
      case _: BooleanSchema                  => "boolean"
      case _: UntitledSingleSelectEnumSchema => "untitled-single"
      case _: TitledSingleSelectEnumSchema   => "titled-single"
      case _: LegacyTitledEnumSchema         => "legacy"
      case _: UntitledMultiSelectEnumSchema  => "untitled-multi"
      case _: TitledMultiSelectEnumSchema    => "titled-multi"

  private def label(message: ClientMessage): String =
    message match
      case _: ClientMessage.Request      => "request"
      case _: ClientMessage.Notification => "notification"
      case _: ClientMessage.Response     => "response"
      case _: ClientMessage.Error        => "error"

  test("fuzz: the generators actually reach every branch they claim to"):
    // A fuzz suite that only ever built TextContent would pass every assertion above and prove
    // nothing, so the shapes are counted. This is the test that caught the generator drawing from
    // the low bits of the LCG, where `below(2)` alternates rather than chooses.
    val rng = Rng(0x5eed_0501)
    val blocks = Vector.fill(500)(rng.contentBlock(2)).map(_.tag).distinct.sorted
    assertEquals(blocks, ContentBlock.Tags.sorted)
    val sampling = Vector.fill(500)(rng.samplingBlock(2)).map(_.tag).distinct.sorted
    assertEquals(sampling, SamplingMessageContentBlock.Tags.sorted)
    val schemas = Vector.fill(500)(rng.primitiveSchema()).map(label).distinct.sorted
    assertEquals(
      schemas,
      Vector(
        "boolean",
        "legacy",
        "number",
        "string",
        "titled-multi",
        "titled-single",
        "untitled-multi",
        "untitled-single"
      )
    )
    val messages = Vector.fill(500)(rng.clientMessage()).map(label).distinct.sorted
    assertEquals(messages, Vector("error", "notification", "request", "response"))
    val requests = Vector.fill(500)(rng.clientRequest()).map(_.method).distinct.sorted
    assertEquals(requests, Vector("initialize", "logging/setLevel", "ping", "tools/call", "tools/list"))
    val serverRequests = Vector.fill(500)(rng.serverRequest()).map(_.method).distinct.sorted
    assertEquals(serverRequests, Vector("elicitation/create", "ping", "sampling/createMessage"))
    val notifications = Vector.fill(500)(rng.serverNotification()).map(_.method).distinct.length
    assertEquals(notifications, 5)
    val clientNotifications = Vector.fill(500)(rng.clientNotification()).map(_.method).distinct.length
    assertEquals(clientNotifications, 3)

  test("fuzz: an option really is optional, in both directions"):
    // The concrete symptom of the low-bit weakness: with `below(2)` off bit 0, this ran Some, None,
    // Some, None forever and every generated record had exactly the same fields present.
    val rng = Rng(0x5eed_0502)
    val flips = Vector.fill(400)(rng.flip())
    assert(flips.contains(true) && flips.contains(false))
    assert(flips.sliding(2).exists(pair => pair(0) == pair(1)), "flip() is alternating, not choosing")
    val ratio = flips.count(identity).toDouble / flips.length
    assert(ratio > 0.35 && ratio < 0.65, s"flip() is lopsided: $ratio")
