package ame

import optparse.{ParseError, compile}

/** Parser-level tests: what an argument vector turns into, and what the user is told when it does
  * not turn into anything.
  *
  * These go through the compiled parser rather than the interpreter, so they also stand as the
  * check that the tree in [[Cli]] survives macro compilation.
  */
class CliSuite extends munit.FunSuite:

  private val parser = compile(Cli.cli)

  private def parse(args: String*): Either[ParseError, Command] = parser.parse(args)

  private val syncSubcommands = List("begin", "commit", "abort", "list", "log", "diff")

  private def help(result: Either[ParseError, Command])(using munit.Location): String =
    result match
      case Left(ParseError.HelpRequested(text)) => text
      case other                                => fail(s"expected help, got $other")

  // -------------------------------------------------------------------------------------------
  // Commands
  // -------------------------------------------------------------------------------------------

  test("commands: version"):
    assertEquals(parse("version"), Right(Command.Version))

  test("commands: sync begin"):
    assertEquals(parse("sync", "begin"), Right(Command.SyncBegin))

  test("commands: sync commit"):
    assertEquals(parse("sync", "commit"), Right(Command.SyncCommit))

  test("commands: sync abort"):
    assertEquals(parse("sync", "abort"), Right(Command.SyncAbort))

  test("commands: sync list"):
    assertEquals(parse("sync", "list"), Right(Command.SyncList))

  test("commands: sync log"):
    assertEquals(parse("sync", "log"), Right(Command.SyncLog))

  test("commands: sync diff"):
    assertEquals(parse("sync", "diff"), Right(Command.SyncDiff))

  // -------------------------------------------------------------------------------------------
  // Missing and unknown subcommands
  // -------------------------------------------------------------------------------------------

  /** The motivating bug (A10.1): `ame sync` used to answer with the root help, which lists `version`
    * and `sync` — everything except the six commands the user was actually being asked for. Each of
    * these pins the carried help against the `--help` text of the scope that raised the error, which
    * is the property that makes the answer useful.
    */
  test("subcommands: an empty argv asks for one of the root commands"):
    assertEquals(
      parse(),
      Left(ParseError.MissingSubcommand(List("version", "sync"), help(parse("--help"))))
    )

  test("subcommands: bare sync asks for one of its six, and shows sync's help"):
    assertEquals(
      parse("sync"),
      Left(ParseError.MissingSubcommand(syncSubcommands, help(parse("sync", "--help"))))
    )

  test("subcommands: an unknown sync subcommand lists the ones that exist"):
    assertEquals(
      parse("sync", "frobnicate"),
      Left(
        ParseError.UnknownSubcommand(
          "frobnicate",
          syncSubcommands,
          help(parse("sync", "--help"))
        )
      )
    )

  test("subcommands: an unknown root command lists the ones that exist"):
    assertEquals(
      parse("frobnicate"),
      Left(
        ParseError.UnknownSubcommand("frobnicate", List("version", "sync"), help(parse("--help")))
      )
    )

  test("subcommands: sync's error help is sync's, not the root's"):
    // The regression this whole ruling exists for: the two must not be the same text.
    val syncHelp = help(parse("sync", "--help"))
    assertNotEquals(syncHelp, help(parse("--help")))
    assert(syncHelp.contains("Begin a new generation (records the pre-sync snapshot)"), syncHelp)
    assert(!syncHelp.contains("Print the version number"), syncHelp)

  // -------------------------------------------------------------------------------------------
  // Help
  // -------------------------------------------------------------------------------------------

  test("help: the root scope lists both commands"):
    val text = help(parse("--help"))
    assert(text.contains("version"), clue(text))
    assert(text.contains("sync"), clue(text))
    assert(text.contains("Manage sync generations"), clue(text))

  test("help: the sync scope lists all five subcommands"):
    val text = help(parse("sync", "--help"))
    syncSubcommands.foreach(name => assert(text.contains(name), clue(text)))
    assert(text.contains("Begin a new generation (records the pre-sync snapshot)"), clue(text))
    assert(text.contains("Show generation details, newest first"), clue(text))
    assert(text.contains("Show changes since the last completed generation"), clue(text))

  /** A scope's help describes what that scope contains, so `begin` — which contains nothing but its
    * own `--help` — renders a bare block. What makes it begin's help rather than sync's is that the
    * siblings and the Commands section are absent from it.
    */
  test("help: a leaf scope answers for itself, not for its parent"):
    val text = help(parse("sync", "begin", "--help"))
    assert(text.contains("--help"), clue(text))
    assert(!text.contains("Commands"), clue(text))
    syncSubcommands.foreach(name => assert(!text.contains(name), clue(text)))
    assertNotEquals(text, help(parse("sync", "--help")))

  test("help: diff answers for its own scope too"):
    val text = help(parse("sync", "diff", "--help"))
    assert(text.contains("--help"), clue(text))
    assert(!text.contains("Commands"), clue(text))
    assertNotEquals(text, help(parse("sync", "--help")))

  test("help: version opted out, so --help there is an unknown option"):
    assertEquals(parse("version", "--help"), Left(ParseError.UnknownOption("--help")))

  // -------------------------------------------------------------------------------------------
  // Leaves take nothing
  // -------------------------------------------------------------------------------------------

  test("arguments: a leaf command takes no positionals"):
    assertEquals(parse("sync", "begin", "now"), Left(ParseError.UnexpectedArgument("now")))

  test("arguments: a leaf command takes no options"):
    assertEquals(parse("sync", "list", "--all"), Left(ParseError.UnknownOption("--all")))
