package optparse

import Scenarios.*

/** Every scenario, compiled once.
  *
  * Hoisting matters: each `compile` call expands into a whole specialized parser, so calling it per
  * test would multiply the generated code for no benefit. These vals also double as the phase-2
  * counterpart of `Scenarios`' no-ascription rule — the shape survives a plain `val` on both sides.
  */
object CompiledScenarios:
  val flagsOnly = compile(Scenarios.flagsOnly)
  val typedOpts = compile(Scenarios.typedOpts)
  val defaultsAndOptional = compile(Scenarios.defaultsAndOptional)
  val mappedOverDefault = compile(Scenarios.mappedOverDefault)
  val mappedUnderDefault = compile(Scenarios.mappedUnderDefault)
  val repeatedOpt = compile(Scenarios.repeatedOpt)
  val mappedUnderRepeated = compile(Scenarios.mappedUnderRepeated)
  val twoRequiredPositionals = compile(Scenarios.twoRequiredPositionals)
  val positionalWithDefault = compile(Scenarios.positionalWithDefault)
  val positionalRepeatedLast = compile(Scenarios.positionalRepeatedLast)
  val optionsAndPositionals = compile(Scenarios.optionsAndPositionals)
  val numericPositionals = compile(Scenarios.numericPositionals)
  val withPure = compile(Scenarios.withPure)
  val mappedCaseClass = compile(Scenarios.mappedCaseClass)
  val gitClone = compile(Scenarios.gitClone)
  val gitRemote = compile(Scenarios.gitRemote)
  val git = compile(Scenarios.git)
  val dockerRun = compile(Scenarios.dockerRun)
  val docker = compile(Scenarios.docker)
  val unionResult = compile(Scenarios.unionResult)

/** The same semantics [[InterpSuite]] pins for the reference interpreter, now through the
  * macro-compiled parsers: section for section, expectation for expectation. The structural-rule
  * section has no counterpart here — those trees are rejected at compile time, see
  * [[CompileErrorSuite]].
  */
class CompileSuite extends munit.FunSuite:
  import CompiledScenarios as C

  private def p[R](parser: Parser[R], args: String*): Either[ParseError, R] =
    parser.parse(args)

  // -------------------------------------------------------------------------------------------
  // Flags
  // -------------------------------------------------------------------------------------------

  test("flags: all absent parse to false"):
    assertEquals(p(C.flagsOnly), Right(((false, false), false)))

  test("flags: long and short forms set the flag"):
    assertEquals(p(C.flagsOnly, "--verbose", "-q"), Right(((true, true), false)))

  test("flags: a flag with no short name is set by its long form"):
    assertEquals(p(C.flagsOnly, "--force"), Right(((false, false), true)))

  test("flags: repetition is idempotent"):
    assertEquals(p(C.flagsOnly, "-v", "--verbose", "-v"), Right(((true, false), false)))

  test("flags: unknown long option is reported as written"):
    assertEquals(p(C.flagsOnly, "--frobnicate"), Left(ParseError.UnknownOption("--frobnicate")))

  test("flags: unknown short option is reported as written"):
    assertEquals(p(C.flagsOnly, "-x"), Left(ParseError.UnknownOption("-x")))

  test("flags (A1): --flag=value is InvalidValue, not UnknownOption"):
    assertEquals(
      p(C.flagsOnly, "--verbose=true"),
      Left(ParseError.InvalidValue("verbose", "true", "flag does not take a value"))
    )

  test("flags (A1): --flag= with empty value is InvalidValue with empty value"):
    assertEquals(
      p(C.flagsOnly, "--force="),
      Left(ParseError.InvalidValue("force", "", "flag does not take a value"))
    )

  test("flags (A2): unknown option with =value keeps the whole token"):
    assertEquals(p(C.flagsOnly, "--frob=1"), Left(ParseError.UnknownOption("--frob=1")))

  test("flags (A6): short-option bundling is an unknown option, token as written"):
    assertEquals(p(C.flagsOnly, "-vq"), Left(ParseError.UnknownOption("-vq")))

  // -------------------------------------------------------------------------------------------
  // Options with values
  // -------------------------------------------------------------------------------------------

  private val allTyped =
    Seq("-j", "4", "-r", "0.5", "-c", "true", "--seed", "123", "-n", "x")

  test("opts: every Read instance decodes via long/short/= forms"):
    assertEquals(p(C.typedOpts, allTyped*), Right(((((4, 0.5), true), 123L), "x")))

  test("opts: --name=value binds the value directly"):
    assertEquals(
      p(C.typedOpts, "--jobs=4", "--ratio=0.5", "--cache=false", "--seed=9", "--name=hi"),
      Right(((((4, 0.5), false), 9L), "hi"))
    )

  test("opts: missing required option in declaration order"):
    assertEquals(p(C.typedOpts), Left(ParseError.MissingOption("jobs")))

  test("opts: invalid Int value via short form reports the long name"):
    assertEquals(
      p(C.typedOpts, "-j", "abc"),
      Left(ParseError.InvalidValue("jobs", "abc", "expected an integer, got 'abc'"))
    )

  test("opts: invalid Boolean value"):
    assertEquals(
      p(C.typedOpts, "-j", "1", "-c", "yes"),
      Left(ParseError.InvalidValue("cache", "yes", "expected 'true' or 'false', got 'yes'"))
    )

  test("opts: invalid Double value"):
    assertEquals(
      p(C.typedOpts, "-j", "1", "-r", "much"),
      Left(ParseError.InvalidValue("ratio", "much", "expected a number, got 'much'"))
    )

  test("opts: invalid Long value"):
    assertEquals(
      p(C.typedOpts, "-j", "1", "--seed", "9.5"),
      Left(ParseError.InvalidValue("seed", "9.5", "expected a long, got '9.5'"))
    )

  test("opts: duplicate non-repeated option (mixed spellings) reports the long name"):
    assertEquals(p(C.typedOpts, "-j", "1", "--jobs", "2"), Left(ParseError.DuplicateOption("jobs")))

  test("opts: missing value at end of args, long form"):
    assertEquals(p(C.typedOpts, "--name"), Left(ParseError.MissingValue("name")))

  test("opts: missing value at end of args, short form reports the long name"):
    assertEquals(p(C.typedOpts, "-j"), Left(ParseError.MissingValue("jobs")))

  test("opts (A6): -c=v is an unknown option, token as written"):
    assertEquals(p(C.typedOpts, "-j=4"), Left(ParseError.UnknownOption("-j=4")))

  test("opts: the token after an option is consumed unconditionally, even dash-leading"):
    assertEquals(p(C.defaultsAndOptional, "--out", "-v"), Right(((1, Some("-v")), false)))

  test("opts: --name= binds the empty string"):
    assertEquals(p(C.defaultsAndOptional, "--out="), Right(((1, Some("")), false)))

  // -------------------------------------------------------------------------------------------
  // Defaults, optional, mapped
  // -------------------------------------------------------------------------------------------

  test("defaults: absent options yield default and None"):
    assertEquals(p(C.defaultsAndOptional), Right(((1, None), false)))

  test("defaults: supplied values override"):
    assertEquals(
      p(C.defaultsAndOptional, "-j", "8", "-o", "out", "-v"),
      Right(((8, Some("out")), true))
    )

  test("map above Default applies to the default itself"):
    assertEquals(p(C.mappedOverDefault), Right(10))
    assertEquals(p(C.mappedOverDefault, "--jobs", "4"), Right(40))

  test("map under Default is skipped when the default is used"):
    assertEquals(p(C.mappedUnderDefault), Right(-1))
    assertEquals(p(C.mappedUnderDefault, "-j", "4"), Right(40))

  // -------------------------------------------------------------------------------------------
  // Repeated options
  // -------------------------------------------------------------------------------------------

  test("repeated: zero occurrences yield Nil"):
    assertEquals(p(C.repeatedOpt), Right((Nil, false)))

  test("repeated: occurrences collect in order across spellings"):
    assertEquals(
      p(C.repeatedOpt, "-I", "a", "--include", "b", "--include=c", "-s"),
      Right((List("a", "b", "c"), true))
    )

  test("repeated: an invalid occurrence fails with the long name"):
    assertEquals(
      p(C.mappedUnderRepeated, "-n", "1", "-n", "two"),
      Left(ParseError.InvalidValue("n", "two", "expected an integer, got 'two'"))
    )

  test("repeated: map under Repeated applies per occurrence"):
    assertEquals(p(C.mappedUnderRepeated, "-n", "1", "-n", "2"), Right(List(2, 3)))
    assertEquals(p(C.mappedUnderRepeated), Right(Nil))

  // -------------------------------------------------------------------------------------------
  // Positionals
  // -------------------------------------------------------------------------------------------

  test("positionals: filled in declaration order"):
    assertEquals(p(C.twoRequiredPositionals, "a", "b"), Right(("a", "b")))

  test("positionals: missing required positional"):
    assertEquals(p(C.twoRequiredPositionals, "a"), Left(ParseError.MissingArgument("dest")))

  test("positionals: extra positional with no slot"):
    assertEquals(
      p(C.twoRequiredPositionals, "a", "b", "c"),
      Left(ParseError.UnexpectedArgument("c"))
    )

  test("positionals: defaulted positional absent"):
    assertEquals(p(C.positionalWithDefault, "f"), Right(("f", 10)))

  test("positionals: defaulted positional supplied"):
    assertEquals(p(C.positionalWithDefault, "f", "3"), Right(("f", 3)))

  test("positionals: invalid value reports the arg name"):
    assertEquals(
      p(C.positionalWithDefault, "f", "xyz"),
      Left(ParseError.InvalidValue("count", "xyz", "expected an integer, got 'xyz'"))
    )

  test("positionals: repeated last collects the rest"):
    assertEquals(p(C.positionalRepeatedLast, "sh", "-", "b"), Right(("sh", List("-", "b"))))

  test("positionals: repeated last may be empty"):
    assertEquals(p(C.positionalRepeatedLast, "sh"), Right(("sh", Nil)))

  test("positionals: options interleave freely with positionals"):
    assertEquals(p(C.optionsAndPositionals, "--jobs", "2", "T", "-v"), Right(((true, 2), "T")))

  test("positionals: bare - and -<digit> are positional-looking"):
    assertEquals(p(C.numericPositionals, "-", "-2"), Right(((0, "-"), -2)))

  test("positionals: a dash-digit token fills a slot even mid-scan"):
    assertEquals(p(C.numericPositionals, "x", "-5"), Right(((0, "x"), -5)))

  test("positionals: an option value may be negative"):
    assertEquals(p(C.numericPositionals, "-o", "-3", "x"), Right(((-3, "x"), 1)))

  // -------------------------------------------------------------------------------------------
  // `--` terminator
  // -------------------------------------------------------------------------------------------

  test("--: everything after is positional"):
    assertEquals(p(C.twoRequiredPositionals, "--", "--src", "--dst"), Right(("--src", "--dst")))

  test("--: only the first -- terminates; later ones are ordinary positionals"):
    assertEquals(p(C.twoRequiredPositionals, "--", "a", "--"), Right(("a", "--")))

  test("-- (A3): dispatch still happens on the token after --"):
    val cli = flag("v", 'v', "d") ~ (sub("build", "d", pure(1)) | sub("test", "d", pure(2)))
    val parser = compile(cli)
    assertEquals(p(parser, "--", "build"), Right((false, 1)))

  test("-- (A3): the inner scope starts with fresh -- state"):
    val cli = sub("build", "d", flag("x", 'x', "d"))
    val parser = compile(cli)
    assertEquals(p(parser, "--", "build", "-x"), Right(true))

  // -------------------------------------------------------------------------------------------
  // pure and map
  // -------------------------------------------------------------------------------------------

  test("pure: consumes nothing and yields its value"):
    assertEquals(p(C.withPure, "--on"), Right((42, true)))
    assertEquals(p(C.withPure), Right((42, false)))

  test("map: builds a case class from nested pairs"):
    assertEquals(p(C.mappedCaseClass, "T", "-j", "2", "-v"), Right(BuildConfig("T", 2, true)))
    assertEquals(p(C.mappedCaseClass, "T"), Right(BuildConfig("T", 1, false)))

  // -------------------------------------------------------------------------------------------
  // Subcommands (git-like)
  // -------------------------------------------------------------------------------------------

  test("sub: dispatch with inner options and positionals"):
    assertEquals(
      p(C.git, "clone", "https://x", "-q"),
      Right(Git(false, Clone("https://x", None, true)))
    )

  test("sub: inner optional option"):
    assertEquals(
      p(C.git, "clone", "repo", "--depth", "3"),
      Right(Git(false, Clone("repo", Some(3), false)))
    )

  test("sub: global option before the dispatch token"):
    assertEquals(
      p(C.git, "-v", "remote", "add", "origin", "u"),
      Right(Git(true, RemoteAdd("origin", "u")))
    )

  test("sub: two levels of nesting"):
    assertEquals(p(C.git, "remote", "remove", "origin"), Right(Git(false, RemoteRemove("origin"))))
    assertEquals(p(C.git, "remote", "list"), Right(Git(false, RemoteList)))

  test("sub: outer options are not recognized after the dispatch token"):
    assertEquals(p(C.git, "clone", "repo", "--verbose"), Left(ParseError.UnknownOption("--verbose")))
    assertEquals(p(C.git, "clone", "repo", "-v"), Left(ParseError.UnknownOption("-v")))

  test("sub: unknown subcommand lists the expected names"):
    assertEquals(p(C.git, "push"), Left(ParseError.UnknownSubcommand("push", List("clone", "remote"))))

  test("sub: unknown nested subcommand"):
    assertEquals(
      p(C.git, "remote", "frob"),
      Left(ParseError.UnknownSubcommand("frob", List("add", "remove", "list")))
    )

  test("sub: missing subcommand at the root"):
    assertEquals(p(C.git, "-v"), Left(ParseError.MissingSubcommand(List("clone", "remote"))))

  test("sub: missing nested subcommand"):
    assertEquals(
      p(C.git, "remote"),
      Left(ParseError.MissingSubcommand(List("add", "remove", "list")))
    )

  test("sub: a lone Sub is a group of one"):
    assertEquals(p(C.gitClone, "clone", "repo"), Right(Clone("repo", None, false)))
    assertEquals(p(C.gitClone, "fetch"), Left(ParseError.UnknownSubcommand("fetch", List("clone"))))

  test("sub: a nested group parses standalone"):
    assertEquals(p(C.gitRemote, "remote", "add", "o", "u"), Right(RemoteAdd("o", "u")))
    assertEquals(p(C.gitRemote, "remote"), Left(ParseError.MissingSubcommand(List("add", "remove", "list"))))

  test("sub: union result types choose per branch"):
    assertEquals(p(C.unionResult, "count", "-n", "5"), Right(5))
    assertEquals(p(C.unionResult, "label", "hi"), Right("hi"))

  test("sub: errors inside a branch surface unchanged"):
    assertEquals(p(C.unionResult, "count"), Left(ParseError.MissingOption("value")))

  test("sub: branch-local maps compose with a map over the whole group"):
    // No scenario exercises a `map` on a OneOf *branch*, which is the one place the compiler has
    // to thread per-branch wrappers rather than a single result map.
    val cli =
      (sub("a", "d", opt[Int]("n", 'n', "d")).map(v => s"a$v") |
        sub("b", "d", sub("x", "d", pure(1)).map(v => s"x$v") | sub("y", "d", pure(2)))
          .map(v => s"b$v")).map(v => s"[$v]")
    val parser = compile(cli)
    assertEquals(p(parser, "a", "-n", "3"), Right("[a3]"))
    assertEquals(p(parser, "b", "x"), Right("[bx1]"))
    assertEquals(p(parser, "b", "y"), Right("[b2]"))
    assertEquals(p(parser, "b"), Left(ParseError.MissingSubcommand(List("x", "y"))))

  // -------------------------------------------------------------------------------------------
  // Docker-like: repeated options plus repeated trailing positionals
  // -------------------------------------------------------------------------------------------

  test("docker: repeated options, flags, and trailing rest"):
    assertEquals(
      p(C.docker, "run", "-e", "A=1", "-e", "B=2", "--detach", "img", "c1", "c2"),
      Right(DockerRun("img", List("c1", "c2"), List("A=1", "B=2"), true, None))
    )

  test("docker: optional name via = form, empty rest"):
    assertEquals(
      p(C.docker, "run", "--name=web", "img"),
      Right(DockerRun("img", Nil, Nil, false, Some("web")))
    )

  test("docker: missing required positional inside the subcommand"):
    assertEquals(p(C.docker, "run", "-d"), Left(ParseError.MissingArgument("image")))

  test("docker: the inner scope also parses on its own"):
    // `-c` only reaches the repeated positional after `--`; before it, it is an unknown short.
    assertEquals(
      p(C.dockerRun, "-e", "A=1", "img", "sh", "--", "-c"),
      Right(DockerRun("img", List("sh", "-c"), List("A=1"), false, None))
    )
    assertEquals(p(C.dockerRun, "-e", "A=1", "img", "sh", "-c"), Left(ParseError.UnknownOption("-c")))

  // -------------------------------------------------------------------------------------------
  // Error precedence (§5): token-level errors beat missing-checks
  // -------------------------------------------------------------------------------------------

  test("precedence: unknown option wins over missing required options"):
    assertEquals(p(C.typedOpts, "--frob"), Left(ParseError.UnknownOption("--frob")))

  test("precedence: unknown subcommand wins over outer missing options"):
    val cli = opt[Int]("n", "d") ~ (sub("a", "d", pure(1)) | sub("b", "d", pure(2)))
    val parser = compile(cli)
    assertEquals(p(parser, "bogus"), Left(ParseError.UnknownSubcommand("bogus", List("a", "b"))))

  test("precedence (A7): missing-checks run in declaration order"):
    val cli = opt[Int]("n", "d") ~ (sub("a", "d", pure(1)) | sub("b", "d", pure(2)))
    val parser = compile(cli)
    assertEquals(p(parser), Left(ParseError.MissingOption("n")))

  // -------------------------------------------------------------------------------------------
  // Help rendering through the compiled parser
  // -------------------------------------------------------------------------------------------

  test("help: options, shorts, and argument forms appear"):
    val text = C.dockerRun.help
    assert(text.contains("--env"), text)
    assert(text.contains("-e"), text)
    assert(text.contains("<image>"), text)
    assert(text.contains("<command>..."), text)
    assert(text.contains("Set an environment variable"), text)

  test("help: subcommands render as a command list"):
    val text = C.git.help
    assert(text.contains("<command>"), text)
    assert(text.contains("clone"), text)
    assert(text.contains("remote"), text)
    assert(text.contains("--verbose"), text)

  test("help: defaulted positionals render bracketed"):
    val text = C.positionalWithDefault.help
    assert(text.contains("[<count>]"), text)
