package optparse

/** The [[ParseError]] reporting surface (A10.4).
  *
  * `message` is a contract, not a `toString`: it is what a command line prints above the help, so
  * every case is pinned here rather than left to whatever the enum's derived rendering happens to
  * produce.
  */
class ErrorsSuite extends munit.FunSuite:

  test("errors (A10.4): every case has a one-line message, in the terms the user typed"):
    assertEquals(ParseError.HelpRequested("...").message, "help requested")
    assertEquals(ParseError.UnknownOption("--frob").message, "unknown option '--frob'")
    assertEquals(ParseError.MissingOption("jobs").message, "missing required option '--jobs'")
    assertEquals(ParseError.MissingValue("jobs").message, "option '--jobs' requires a value")
    assertEquals(
      ParseError.DuplicateOption("jobs").message,
      "option '--jobs' may only be given once"
    )
    assertEquals(
      ParseError.InvalidValue("jobs", "abc", "expected an integer, got 'abc'").message,
      "invalid value 'abc' for 'jobs': expected an integer, got 'abc'"
    )
    assertEquals(ParseError.MissingArgument("dest").message, "missing argument 'dest'")
    assertEquals(ParseError.UnexpectedArgument("stray").message, "unexpected argument 'stray'")
    assertEquals(
      ParseError.UnknownSubcommand("push", List("clone", "remote"), "H").message,
      "unknown command 'push'"
    )
    assertEquals(ParseError.MissingSubcommand(List("clone", "remote"), "H").message, "missing command")

  test("errors (A10.4/A10.6): InvalidValue composes its line, detail and all"):
    // `message` composes a sentence; it does not forward InvalidValue's own field, which is what an
    // `override val message` would have silently reduced it to. The detail here is a real `Read`
    // failure naming both the raw token and the reason, so no message built out of `name` and
    // `value` alone can reproduce this line, and dropping the detail cannot pass either.
    val detail = Read[Int].read("abc") match
      case Left(reason)  => reason
      case Right(parsed) => fail(s"expected 'abc' to be an invalid Int, got $parsed")
    assertEquals(detail, "expected an integer, got 'abc'")

    val error = ParseError.InvalidValue("jobs", "abc", detail)
    assertEquals(error.message, "invalid value 'abc' for 'jobs': expected an integer, got 'abc'")
    assert(error.message.contains(detail), error.message)
    assertNotEquals(error.message, detail)

  test("errors (A10.4): the messages name no help text and list no expected commands"):
    // The Commands section of `contextHelp` is what lists the alternatives; repeating them in the
    // message would print them twice.
    val unknown = ParseError.UnknownSubcommand("push", List("clone", "remote"), "H")
    assert(!unknown.message.contains("clone"), unknown.message)
    assert(!unknown.message.contains("H"), unknown.message)
    val missing = ParseError.MissingSubcommand(List("clone", "remote"), "H")
    assert(!missing.message.contains("clone"), missing.message)
    assert(!missing.message.contains("H"), missing.message)

  test("errors (A10.4): contextHelp is the carried scope help, for the two cases that have one"):
    assertEquals(ParseError.UnknownSubcommand("push", Nil, "scope help").contextHelp, Some("scope help"))
    assertEquals(ParseError.MissingSubcommand(Nil, "scope help").contextHelp, Some("scope help"))

  test("errors (A10.4): no other case carries context help"):
    // HelpRequested included: its text is the answer, not the context for one.
    assertEquals(ParseError.HelpRequested("text").contextHelp, None)
    assertEquals(ParseError.UnknownOption("--frob").contextHelp, None)
    assertEquals(ParseError.MissingOption("jobs").contextHelp, None)
    assertEquals(ParseError.MissingValue("jobs").contextHelp, None)
    assertEquals(ParseError.DuplicateOption("jobs").contextHelp, None)
    assertEquals(ParseError.InvalidValue("jobs", "abc", "bad").contextHelp, None)
    assertEquals(ParseError.MissingArgument("dest").contextHelp, None)
    assertEquals(ParseError.UnexpectedArgument("stray").contextHelp, None)
