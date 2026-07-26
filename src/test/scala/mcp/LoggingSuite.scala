package mcp

/** Log levels and the two messages that carry them (schema.ts:1500-1570). */
class LoggingSuite extends McpSuite:

  test("levels: all eight RFC-5424 severities, spelled as the schema spells them"):
    assertEquals(
      LoggingLevel.values.map(_.wire).toVector,
      Vector("debug", "info", "notice", "warning", "error", "critical", "alert", "emergency")
    )
    for level <- LoggingLevel.values do
      assertGolden(s""""${level.wire}"""", level, LoggingLevel.decoder, LoggingLevel.encoder)

  test("levels: severity counts up from debug, which is what 'this level and higher' means"):
    assertEquals(LoggingLevel.Debug.severity, 0)
    assertEquals(LoggingLevel.Emergency.severity, 7)
    assert(LoggingLevel.Warning.severity > LoggingLevel.Info.severity)
    assert(LoggingLevel.Critical.severity > LoggingLevel.Error.severity)
    assertEquals(LoggingLevel.values.toVector.map(_.severity), (0 to 7).toVector)

  test("levels: an unrecognised level is refused with the alternatives listed"):
    assertEquals(
      rendered("\"verbose\"", LoggingLevel.decoder),
      """$: expected one of "debug", "info", "notice", "warning", "error", "critical", "alert", "emergency", found "verbose""""
    )
    assertEquals(rendered("\"DEBUG\"", LoggingLevel.decoder).contains("found \"DEBUG\""), true)

  test("logging/setLevel params: the level, and the meta beside it"):
    assertGolden(
      Golden.setLevel,
      SetLevelRequestParams(None, LoggingLevel.Warning),
      SetLevelRequestParams.decoder,
      SetLevelRequestParams.encoder
    )
    assertGolden(
      s"""{"_meta":${Golden.meta},"level":"alert"}""",
      SetLevelRequestParams(Some(Sample.meta), LoggingLevel.Alert),
      SetLevelRequestParams.decoder,
      SetLevelRequestParams.encoder
    )

  test("logging/setLevel params: the level is required"):
    assertEquals(rendered("{}", SetLevelRequestParams.decoder), "$.level: missing required field")

  test("notifications/message params: data is required and may be any JSON at all"):
    assertGolden(
      Golden.loggingMinimal,
      Sample.loggingMinimal,
      LoggingMessageNotificationParams.decoder,
      LoggingMessageNotificationParams.encoder
    )
    assertGolden(
      Golden.loggingFull,
      Sample.loggingFull,
      LoggingMessageNotificationParams.decoder,
      LoggingMessageNotificationParams.encoder
    )

  test("notifications/message params: null data is the value null, not an absence"):
    // The one required field in the package that reads `null` as something rather than as an error.
    assertEquals(
      decoded("""{"level":"info","data":null}""", LoggingMessageNotificationParams.decoder).data,
      Json.Null
    )
    assertEquals(
      rendered("""{"level":"info"}""", LoggingMessageNotificationParams.decoder),
      "$.data: missing required field"
    )

  test("notifications/message params: data of every shape survives unchanged"):
    for shape <- Vector("null", "true", "1.5", "\"text\"", "[1,[2],{}]", """{"a":{"b":[null]}}""") do
      val text = s"""{"level":"info","data":$shape}"""
      assertEquals(
        Codec.encode(
          decoded(text, LoggingMessageNotificationParams.decoder),
          LoggingMessageNotificationParams.encoder
        ),
        text
      )

  test("notifications/message params: the logger name is optional"):
    assertEquals(
      decoded("""{"level":"info","logger":null,"data":1}""", LoggingMessageNotificationParams.decoder).logger,
      None
    )
    assertEquals(
      decoded("""{"level":"info","logger":"db","data":1}""", LoggingMessageNotificationParams.decoder).logger,
      Some("db")
    )
