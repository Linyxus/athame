package optparse

import Scenarios.*

/** Semantics tests for the reference interpreter, organized by spec section (§5, rulings A1-A7).
  * Phase 2's macro-compiled parsers must reproduce every behavior pinned here bit-for-bit.
  */
class InterpSuite extends munit.FunSuite:

  private def p[R](cli: Cli[?, R], args: String*): Either[ParseError, R] =
    Interp.parse(cli, args)

  // -------------------------------------------------------------------------------------------
  // Flags
  // -------------------------------------------------------------------------------------------

  test("flags: all absent parse to false"):
    assertEquals(p(flagsOnly), Right(((false, false), false)))

  test("flags: long and short forms set the flag"):
    assertEquals(p(flagsOnly, "--verbose", "-q"), Right(((true, true), false)))

  test("flags: a flag with no short name is set by its long form"):
    assertEquals(p(flagsOnly, "--force"), Right(((false, false), true)))

  test("flags: repetition is idempotent"):
    assertEquals(p(flagsOnly, "-v", "--verbose", "-v"), Right(((true, false), false)))

  test("flags: unknown long option is reported as written"):
    assertEquals(p(flagsOnly, "--frobnicate"), Left(ParseError.UnknownOption("--frobnicate")))

  test("flags: unknown short option is reported as written"):
    assertEquals(p(flagsOnly, "-x"), Left(ParseError.UnknownOption("-x")))

  test("flags (A1): --flag=value is InvalidValue, not UnknownOption"):
    assertEquals(
      p(flagsOnly, "--verbose=true"),
      Left(ParseError.InvalidValue("verbose", "true", "flag does not take a value"))
    )

  test("flags (A1): --flag= with empty value is InvalidValue with empty value"):
    assertEquals(
      p(flagsOnly, "--force="),
      Left(ParseError.InvalidValue("force", "", "flag does not take a value"))
    )

  test("flags (A2): unknown option with =value keeps the whole token"):
    assertEquals(p(flagsOnly, "--frob=1"), Left(ParseError.UnknownOption("--frob=1")))

  test("flags (A6): short-option bundling is an unknown option, token as written"):
    assertEquals(p(flagsOnly, "-vq"), Left(ParseError.UnknownOption("-vq")))

  // -------------------------------------------------------------------------------------------
  // Options with values
  // -------------------------------------------------------------------------------------------

  private val allTyped =
    Seq("-j", "4", "-r", "0.5", "-c", "true", "--seed", "123", "-n", "x")

  test("opts: every Read instance decodes via long/short/= forms"):
    assertEquals(p(typedOpts, allTyped*), Right(((((4, 0.5), true), 123L), "x")))

  test("opts: --name=value binds the value directly"):
    assertEquals(
      p(typedOpts, "--jobs=4", "--ratio=0.5", "--cache=false", "--seed=9", "--name=hi"),
      Right(((((4, 0.5), false), 9L), "hi"))
    )

  test("opts: missing required option in declaration order"):
    assertEquals(p(typedOpts), Left(ParseError.MissingOption("jobs")))

  test("opts: invalid Int value via short form reports the long name"):
    assertEquals(
      p(typedOpts, "-j", "abc"),
      Left(ParseError.InvalidValue("jobs", "abc", "expected an integer, got 'abc'"))
    )

  test("opts: invalid Boolean value"):
    assertEquals(
      p(typedOpts, "-j", "1", "-c", "yes"),
      Left(ParseError.InvalidValue("cache", "yes", "expected 'true' or 'false', got 'yes'"))
    )

  test("opts: invalid Double value"):
    assertEquals(
      p(typedOpts, "-j", "1", "-r", "much"),
      Left(ParseError.InvalidValue("ratio", "much", "expected a number, got 'much'"))
    )

  test("opts: invalid Long value"):
    assertEquals(
      p(typedOpts, "-j", "1", "--seed", "9.5"),
      Left(ParseError.InvalidValue("seed", "9.5", "expected a long, got '9.5'"))
    )

  test("opts: duplicate non-repeated option (mixed spellings) reports the long name"):
    assertEquals(p(typedOpts, "-j", "1", "--jobs", "2"), Left(ParseError.DuplicateOption("jobs")))

  test("opts: missing value at end of args, long form"):
    assertEquals(p(typedOpts, "--name"), Left(ParseError.MissingValue("name")))

  test("opts: missing value at end of args, short form reports the long name"):
    assertEquals(p(typedOpts, "-j"), Left(ParseError.MissingValue("jobs")))

  test("opts (A6): -c=v is an unknown option, token as written"):
    assertEquals(p(typedOpts, "-j=4"), Left(ParseError.UnknownOption("-j=4")))

  test("opts: the token after an option is consumed unconditionally, even dash-leading"):
    assertEquals(p(defaultsAndOptional, "--out", "-v"), Right(((1, Some("-v")), false)))

  test("opts: --name= binds the empty string"):
    assertEquals(p(defaultsAndOptional, "--out="), Right(((1, Some("")), false)))

  // -------------------------------------------------------------------------------------------
  // Defaults, optional, mapped
  // -------------------------------------------------------------------------------------------

  test("defaults: absent options yield default and None"):
    assertEquals(p(defaultsAndOptional), Right(((1, None), false)))

  test("defaults: supplied values override"):
    assertEquals(
      p(defaultsAndOptional, "-j", "8", "-o", "out", "-v"),
      Right(((8, Some("out")), true))
    )

  test("map above Default applies to the default itself"):
    assertEquals(p(mappedOverDefault), Right(10))
    assertEquals(p(mappedOverDefault, "--jobs", "4"), Right(40))

  test("map under Default is skipped when the default is used"):
    assertEquals(p(mappedUnderDefault), Right(-1))
    assertEquals(p(mappedUnderDefault, "-j", "4"), Right(40))

  // -------------------------------------------------------------------------------------------
  // Repeated options
  // -------------------------------------------------------------------------------------------

  test("repeated: zero occurrences yield Nil"):
    assertEquals(p(repeatedOpt), Right((Nil, false)))

  test("repeated: occurrences collect in order across spellings"):
    assertEquals(
      p(repeatedOpt, "-I", "a", "--include", "b", "--include=c", "-s"),
      Right((List("a", "b", "c"), true))
    )

  test("repeated: map under Repeated applies per occurrence"):
    assertEquals(p(mappedUnderRepeated, "-n", "1", "-n", "2"), Right(List(2, 3)))
    assertEquals(p(mappedUnderRepeated), Right(Nil))

  // -------------------------------------------------------------------------------------------
  // Positionals
  // -------------------------------------------------------------------------------------------

  test("positionals: filled in declaration order"):
    assertEquals(p(twoRequiredPositionals, "a", "b"), Right(("a", "b")))

  test("positionals: missing required positional"):
    assertEquals(p(twoRequiredPositionals, "a"), Left(ParseError.MissingArgument("dest")))

  test("positionals: extra positional with no slot"):
    assertEquals(p(twoRequiredPositionals, "a", "b", "c"), Left(ParseError.UnexpectedArgument("c")))

  test("positionals: defaulted positional absent"):
    assertEquals(p(positionalWithDefault, "f"), Right(("f", 10)))

  test("positionals: defaulted positional supplied"):
    assertEquals(p(positionalWithDefault, "f", "3"), Right(("f", 3)))

  test("positionals: invalid value reports the arg name"):
    assertEquals(
      p(positionalWithDefault, "f", "xyz"),
      Left(ParseError.InvalidValue("count", "xyz", "expected an integer, got 'xyz'"))
    )

  test("positionals: repeated last collects the rest"):
    assertEquals(p(positionalRepeatedLast, "sh", "-", "b"), Right(("sh", List("-", "b"))))

  test("positionals: repeated last may be empty"):
    assertEquals(p(positionalRepeatedLast, "sh"), Right(("sh", Nil)))

  test("positionals: options interleave freely with positionals"):
    assertEquals(
      p(optionsAndPositionals, "--jobs", "2", "T", "-v"),
      Right(((true, 2), "T"))
    )

  test("positionals: bare - and -<digit> are positional-looking"):
    assertEquals(p(numericPositionals, "-", "-2"), Right(((0, "-"), -2)))

  test("positionals: a dash-digit token fills a slot even mid-scan"):
    assertEquals(p(numericPositionals, "x", "-5"), Right(((0, "x"), -5)))

  test("positionals: an option value may be negative"):
    assertEquals(p(numericPositionals, "-o", "-3", "x"), Right(((-3, "x"), 1)))

  // -------------------------------------------------------------------------------------------
  // `--` terminator
  // -------------------------------------------------------------------------------------------

  test("--: everything after is positional"):
    assertEquals(p(twoRequiredPositionals, "--", "--src", "--dst"), Right(("--src", "--dst")))

  test("--: only the first -- terminates; later ones are ordinary positionals"):
    assertEquals(p(twoRequiredPositionals, "--", "a", "--"), Right(("a", "--")))

  test("-- (A3): dispatch still happens on the token after --"):
    val cli = flag("v", 'v', "d") ~ (sub("build", "d", pure(1)) | sub("test", "d", pure(2)))
    assertEquals(p(cli, "--", "build"), Right((false, 1)))

  test("-- (A3): the inner scope starts with fresh -- state"):
    val cli = sub("build", "d", flag("x", 'x', "d"))
    assertEquals(p(cli, "--", "build", "-x"), Right(true))

  // -------------------------------------------------------------------------------------------
  // pure and map
  // -------------------------------------------------------------------------------------------

  test("pure: consumes nothing and yields its value"):
    assertEquals(p(withPure, "--on"), Right((42, true)))
    assertEquals(p(withPure), Right((42, false)))

  test("map: builds a case class from nested pairs"):
    assertEquals(p(mappedCaseClass, "T", "-j", "2", "-v"), Right(BuildConfig("T", 2, true)))
    assertEquals(p(mappedCaseClass, "T"), Right(BuildConfig("T", 1, false)))

  // -------------------------------------------------------------------------------------------
  // Subcommands (git-like)
  // -------------------------------------------------------------------------------------------

  test("sub: dispatch with inner options and positionals"):
    assertEquals(
      p(git, "clone", "https://x", "-q"),
      Right(Git(false, Clone("https://x", None, true)))
    )

  test("sub: inner optional option"):
    assertEquals(
      p(git, "clone", "repo", "--depth", "3"),
      Right(Git(false, Clone("repo", Some(3), false)))
    )

  test("sub: global option before the dispatch token"):
    assertEquals(
      p(git, "-v", "remote", "add", "origin", "u"),
      Right(Git(true, RemoteAdd("origin", "u")))
    )

  test("sub: two levels of nesting"):
    assertEquals(p(git, "remote", "remove", "origin"), Right(Git(false, RemoteRemove("origin"))))
    assertEquals(p(git, "remote", "list"), Right(Git(false, RemoteList)))

  test("sub: outer options are not recognized after the dispatch token"):
    assertEquals(p(git, "clone", "repo", "--verbose"), Left(ParseError.UnknownOption("--verbose")))
    assertEquals(p(git, "clone", "repo", "-v"), Left(ParseError.UnknownOption("-v")))

  test("sub: unknown subcommand lists the expected names"):
    assertEquals(
      p(git, "push"),
      Left(ParseError.UnknownSubcommand("push", List("clone", "remote")))
    )

  test("sub: unknown nested subcommand"):
    assertEquals(
      p(git, "remote", "frob"),
      Left(ParseError.UnknownSubcommand("frob", List("add", "remove", "list")))
    )

  test("sub: missing subcommand at the root"):
    assertEquals(p(git, "-v"), Left(ParseError.MissingSubcommand(List("clone", "remote"))))

  test("sub: missing nested subcommand"):
    assertEquals(p(git, "remote"), Left(ParseError.MissingSubcommand(List("add", "remove", "list"))))

  test("sub: union result types choose per branch"):
    assertEquals(p(unionResult, "count", "-n", "5"), Right(5))
    assertEquals(p(unionResult, "label", "hi"), Right("hi"))

  test("sub: errors inside a branch surface unchanged"):
    assertEquals(p(unionResult, "count"), Left(ParseError.MissingOption("value")))

  // -------------------------------------------------------------------------------------------
  // Docker-like: repeated options plus repeated trailing positionals
  // -------------------------------------------------------------------------------------------

  test("docker: repeated options, flags, and trailing rest"):
    assertEquals(
      p(docker, "run", "-e", "A=1", "-e", "B=2", "--detach", "img", "c1", "c2"),
      Right(DockerRun("img", List("c1", "c2"), List("A=1", "B=2"), true, None))
    )

  test("docker: optional name via = form, empty rest"):
    assertEquals(
      p(docker, "run", "--name=web", "img"),
      Right(DockerRun("img", Nil, Nil, false, Some("web")))
    )

  test("docker: missing required positional inside the subcommand"):
    assertEquals(p(docker, "run", "-d"), Left(ParseError.MissingArgument("image")))

  // -------------------------------------------------------------------------------------------
  // Error precedence (§5): token-level errors beat missing-checks
  // -------------------------------------------------------------------------------------------

  test("precedence: unknown option wins over missing required options"):
    assertEquals(p(typedOpts, "--frob"), Left(ParseError.UnknownOption("--frob")))

  test("precedence: unknown subcommand wins over outer missing options"):
    val cli = opt[Int]("n", "d") ~ (sub("a", "d", pure(1)) | sub("b", "d", pure(2)))
    assertEquals(p(cli, "bogus"), Left(ParseError.UnknownSubcommand("bogus", List("a", "b"))))

  test("precedence (A7): missing-checks run in declaration order"):
    // required opt is declared before the subcommand group, so it reports first
    val cli = opt[Int]("n", "d") ~ (sub("a", "d", pure(1)) | sub("b", "d", pure(2)))
    assertEquals(p(cli), Left(ParseError.MissingOption("n")))

  // -------------------------------------------------------------------------------------------
  // Automatic --help (A8.1-A8.7)
  // -------------------------------------------------------------------------------------------

  private def helpOf[R](cli: Cli[?, R], args: String*): String =
    p(cli, args*) match
      case Left(ParseError.HelpRequested(text)) => text
      case other                                => fail(s"expected help, got $other")

  test("help (A8.1): --help at the root yields the root scope's rendering"):
    assertEquals(p(flagsOnly, "--help"), Left(ParseError.HelpRequested(Help.render(flagsOnly))))

  test("help (A8.1): --help is accepted anywhere in option position"):
    assertEquals(p(typedOpts, "-j", "4", "--help"), Left(ParseError.HelpRequested(Help.render(typedOpts))))

  test("help (A8.2): after dispatch, --help belongs to the inner scope"):
    // `docker` is a single `sub("run", …, dockerRun)`, so the inner scope is exactly `dockerRun`.
    assertEquals(p(docker, "run", "--help"), Left(ParseError.HelpRequested(Help.render(dockerRun))))

  test("help (A8.2): a subcommand's help describes the subcommand, not its parent"):
    val text = helpOf(git, "clone", "--help")
    assert(text.contains("<repo>"), text)
    assert(text.contains("--depth"), text)
    assert(!text.contains("remote"), text)

  test("help (A8.2): a doubly nested subcommand renders the innermost scope"):
    val text = helpOf(git, "remote", "add", "--help")
    assert(text.contains("name"), text)
    assert(text.contains("url"), text)
    assert(!text.contains("clone"), text)

  test("help (A8.2): the parent scope's help lists its own commands"):
    val text = helpOf(git, "--help")
    assert(text.contains("clone"), text)
    assert(text.contains("remote"), text)
    assert(text.contains("--verbose"), text)

  test("help (A8.3): help wins over missing required positionals"):
    assertEquals(p(docker, "run", "--help"), Left(ParseError.HelpRequested(Help.render(dockerRun))))

  test("help (A8.3): help wins over missing required options"):
    val text = helpOf(unionResult, "count", "--help")
    assert(text.contains("--value"), text)

  test("help (A8.3): an earlier token-level error wins over a later --help"):
    assertEquals(p(flagsOnly, "--bogus", "--help"), Left(ParseError.UnknownOption("--bogus")))

  test("help (A8.3): --help short-circuits before a later bad token"):
    assertEquals(p(flagsOnly, "--help", "--bogus"), Left(ParseError.HelpRequested(Help.render(flagsOnly))))

  test("help (A8.2): after --, --help is an ordinary positional that fills a slot"):
    assertEquals(p(twoRequiredPositionals, "--", "--help", "b"), Right(("--help", "b")))

  test("help (A8.2): after --, --help with no slot is an unexpected argument"):
    assertEquals(p(flagsOnly, "--", "--help"), Left(ParseError.UnexpectedArgument("--help")))

  test("help (A8.2): --help=value is an InvalidValue, like any other flag"):
    assertEquals(
      p(flagsOnly, "--help=x"),
      Left(ParseError.InvalidValue("help", "x", "flag does not take a value"))
    )

  test("help (A8.5): the rendering lists --help beside the scope's own options"):
    val text = helpOf(flagsOnly, "--help")
    assert(text.contains("--help"), text)
    assert(text.contains("Show this help and exit"), text)
    assert(text.contains("--verbose"), text)
    assert(text.contains("--force"), text)

  test("help (A8.5): a grammar with no options of its own still documents --help"):
    val text = helpOf(twoRequiredPositionals, "--help")
    assert(text.contains("--help"), text)
    assert(text.contains("<source>"), text)

  // -------------------------------------------------------------------------------------------
  // Per-subcommand help opt-out (A9.1-A9.7)
  // -------------------------------------------------------------------------------------------

  test("help (A9.2): a help-disabled scope treats --help as an unknown option"):
    assertEquals(p(helpOptOut, "version", "--help"), Left(ParseError.UnknownOption("--help")))

  test("help (A9.2): --help=x in a help-disabled scope is unknown, token as written"):
    // A1 does not apply: `help` is not a known option there, so this is A2, not InvalidValue.
    assertEquals(p(helpOptOut, "version", "--help=x"), Left(ParseError.UnknownOption("--help=x")))

  test("help (A9.2): a help-disabled scope parses normally otherwise"):
    assertEquals(p(helpOptOut, "version"), Right("0.1.0"))
    assertEquals(p(helpOptOut, "version", "extra"), Left(ParseError.UnexpectedArgument("extra")))
    assertEquals(p(helpOptOut, "version", "--", "--help"), Left(ParseError.UnexpectedArgument("--help")))

  test("help (A9.2): a help-enabled sibling is unaffected"):
    val text = helpOf(helpOptOut, "build", "--help")
    assert(text.contains("--fast"), text)
    assert(text.contains("<target>"), text)

  test("help (A9.1): the root scope keeps auto-help beside an opted-out subcommand"):
    val text = helpOf(helpOptOut, "--help")
    assert(text.contains("--help"), text)
    assert(text.contains("version"), text)
    assert(text.contains("build"), text)

  test("help (A9.2): help-ness is not inherited by nested scopes"):
    assertEquals(p(nestedHelpOptOut, "outer", "--help"), Left(ParseError.UnknownOption("--help")))
    val text = helpOf(nestedHelpOptOut, "outer", "inner", "--help")
    assert(text.contains("--depth"), text)

  test("help (A9.2): a silent scope nested inside a silent scope stays silent"):
    assertEquals(
      p(nestedHelpOptOut, "outer", "mute", "--help"),
      Left(ParseError.UnknownOption("--help"))
    )
    assertEquals(p(nestedHelpOptOut, "outer", "mute", "--loud"), Right(true))

  test("help (A9.4): the Commands list does not mark help-disabled subcommands"):
    // Both rows are plain name/description pairs; `version` carries no hint that it is silent.
    val text = Help.render(helpOptOut)
    assert(text.contains("version  Print the version number"), text)
    assert(text.contains("build    Compile a target"), text)

  test("help (A9.4): includeHelpRow = false omits the automatic row"):
    val text = Help.render(flagsOnly, includeHelpRow = false)
    assert(!text.contains("--help"), text)
    assert(!text.contains("Show this help and exit"), text)
    assert(text.contains("--verbose"), text)

  test("help (A9.4): a grammar with no options renders no Options section without the row"):
    val text = Help.render(twoRequiredPositionals, includeHelpRow = false)
    assert(!text.contains("Options:"), text)
    assert(text.contains("<source>"), text)

  // -------------------------------------------------------------------------------------------
  // Structural rules R1-R6 (IllegalArgumentException, eager, argv-independent per A5)
  // -------------------------------------------------------------------------------------------

  private def violation(cli: Cli[?, ?]): String =
    intercept[IllegalArgumentException](Interp.parse(cli, Nil)).getMessage

  test("R1: duplicate long option name"):
    val message = violation(flag("x", "d") ~ opt[Int]("x", "d"))
    assert(message.contains("optparse:"), message)
    assert(message.contains("--x"), message)

  test("R1: duplicate short option name"):
    val message = violation(flag("aa", 'x', "d") ~ flag("bb", 'x', "d"))
    assert(message.contains("-x"), message)

  test("R1 (A4): duplicate Arg names are legal metavars"):
    assertEquals(p(arg[String]("f", "d") ~ arg[String]("f", "d"), "1", "2"), Right(("1", "2")))

  test("R2: duplicate subcommand names in a group"):
    val message = violation(sub("a", "d", pure(1)) | sub("a", "d", pure(2)))
    assert(message.contains("'a'"), message)

  test("R2: a OneOf branch must be subcommand-rooted"):
    val message = violation(flag("x", "d") | sub("b", "d", pure(1)))
    assert(message.contains("subcommand-rooted"), message)

  test("R3: at most one subcommand group per scope"):
    val message = violation(sub("a", "d", pure(1)) ~ sub("b", "d", pure(2)))
    assert(message.contains("at most one subcommand group"), message)

  test("R3: positionals and a subcommand group may not share a scope"):
    val message = violation(arg[String]("x", "d") ~ sub("a", "d", pure(1)))
    assert(message.contains("subcommand group"), message)

  test("R4: Default may not wrap a Flag"):
    val message = violation(flag("x", "d").withDefault(true))
    assert(message.contains("Flag"), message)

  test("R4: Repeated may not wrap a Flag"):
    val message = violation(flag("x", "d").repeated)
    assert(message.contains("Flag"), message)

  test("R4: Repeated and Default may not stack"):
    val message = violation(opt[Int]("x", "d").withDefault(1).repeated)
    assert(message.contains("stacked"), message)

  test("R5: a repeated positional must be last"):
    val message = violation(arg[String]("a", "d").repeated ~ arg[String]("b", "d"))
    assert(message.contains("must be last"), message)

  test("R5: at most one repeated positional"):
    val message = violation(arg[String]("a", "d").repeated ~ arg[String]("b", "d").repeated)
    assert(message.contains("at most one repeated positional"), message)

  test("R5: no required positional after a defaulted one"):
    val message = violation(arg[String]("a", "d").withDefault("x") ~ arg[String]("b", "d"))
    assert(message.contains("may not follow"), message)

  test("R6: a Flag may not be named help"):
    val message = violation(flag("help", 'h', "d"))
    assert(message.contains("optparse:"), message)
    assert(message.contains("'help' is reserved"), message)

  test("R6: an Opt may not be named help"):
    val message = violation(opt[Int]("help", "d"))
    assert(message.contains("'help' is reserved"), message)

  test("R6: the reservation reaches subcommand scopes too"):
    val message = violation(sub("go", "d", flag("help", "d")))
    assert(message.contains("'help' is reserved"), message)

  test("R6 (A9.3): the reservation holds inside a help-disabled scope too"):
    val message = violation(sub("quiet", "d", flag("help", "d"), help = false))
    assert(message.contains("'help' is reserved"), message)

  test("R6 (A9.3): an Opt named help is rejected inside a help-disabled scope"):
    val message = violation(sub("quiet", "d", opt[Int]("help", "d"), help = false))
    assert(message.contains("'help' is reserved"), message)

  test("R6 (A4): an Arg named help is a legal metavar"):
    assertEquals(p(arg[String]("help", "d"), "x"), Right("x"))

  test("R6: a subcommand named help is legal"):
    assertEquals(p(sub("help", "d", pure(1)) | sub("go", "d", pure(2)), "help"), Right(1))

  test("R6: short 'h' is not reserved"):
    assertEquals(p(flag("hop", 'h', "d"), "-h"), Right(true))

  test("A5: validation is eager in subcommand scopes the argv never reaches"):
    val cli = sub("ok", "d", pure(1)) | sub("bad", "d", flag("x", "d") ~ flag("x", "d"))
    intercept[IllegalArgumentException](Interp.parse(cli, Seq("ok")))

  // -------------------------------------------------------------------------------------------
  // Help rendering (sanity only; format is not contractual)
  // -------------------------------------------------------------------------------------------

  test("help: options, shorts, and argument forms appear"):
    val text = Help.render(dockerRun)
    assert(text.contains("--env"), text)
    assert(text.contains("-e"), text)
    assert(text.contains("<image>"), text)
    assert(text.contains("<command>..."), text)
    assert(text.contains("Set an environment variable"), text)

  test("help: subcommands render as a command list"):
    val text = Help.render(git)
    assert(text.contains("<command>"), text)
    assert(text.contains("clone"), text)
    assert(text.contains("remote"), text)
    assert(text.contains("--verbose"), text)

  test("help: defaulted positionals render bracketed"):
    val text = Help.render(positionalWithDefault)
    assert(text.contains("[<count>]"), text)
