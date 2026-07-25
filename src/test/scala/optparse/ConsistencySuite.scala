package optparse

/** The macro and the interpreter must be the same function.
  *
  * For every scenario this suite replays a list of argument vectors — valid and invalid — through
  * both backends and demands exact equality, `Left` values included. The curated vectors below aim
  * at the cases we know are interesting; the fuzz section at the bottom then throws deterministic
  * token soup at both, which is what catches the cases we did not think of.
  */
class ConsistencySuite extends munit.FunSuite:
  import CompiledScenarios as C

  private def agree[R](
    name: String,
    cli: Cli[?, R],
    parser: Parser[R],
    vectors: List[List[String]]
  )(using munit.Location): Unit =
    vectors.foreach { args =>
      assertEquals(
        parser.parse(args),
        Interp.parse(cli, args),
        s"$name disagreed on ${args.mkString("[", ", ", "]")}"
      )
    }

  test("consistency: flagsOnly"):
    agree(
      "flagsOnly",
      Scenarios.flagsOnly,
      C.flagsOnly,
      List(
        Nil,
        List("--verbose"),
        List("-v", "-q", "--force"),
        List("-v", "-v", "--verbose"),
        List("--frobnicate"),
        List("-x"),
        List("-vq"),
        List("--verbose=true"),
        List("--force="),
        List("--frob=1"),
        List("stray"),
        List("--", "stray"),
        List("-"),
        List("-2"),
        List("--help"),
        List("-v", "--help"),
        List("--help", "--frobnicate"),
        List("--frobnicate", "--help"),
        List("--help=1"),
        List("--help="),
        List("--", "--help")
      )
    )

  test("consistency: typedOpts"):
    agree(
      "typedOpts",
      Scenarios.typedOpts,
      C.typedOpts,
      List(
        Nil,
        List("-j", "4", "-r", "0.5", "-c", "true", "--seed", "123", "-n", "x"),
        List("--jobs=4", "--ratio=0.5", "--cache=false", "--seed=9", "--name=hi"),
        List("-j", "abc"),
        List("-j", "1", "-r", "much"),
        List("-j", "1", "-c", "yes"),
        List("-j", "1", "--seed", "9.5"),
        List("-j", "1", "--jobs", "2"),
        List("--name"),
        List("-j"),
        List("-j=4"),
        List("--frob"),
        List("-j", "4", "extra"),
        List("--jobs", "--ratio"),
        List("--help"),
        List("-j", "4", "--help"),
        List("--jobs", "--help"),
        List("--help=x")
      )
    )

  test("consistency: defaultsAndOptional"):
    agree(
      "defaultsAndOptional",
      Scenarios.defaultsAndOptional,
      C.defaultsAndOptional,
      List(
        Nil,
        List("-j", "8", "-o", "out", "-v"),
        List("--jobs=2"),
        List("--out="),
        List("--out", "-v"),
        List("-o"),
        List("-j", "zero"),
        List("-o", "a", "--out", "b"),
        List("--verbose", "--verbose"),
        List("leftover")
      )
    )

  test("consistency: mappedOverDefault and mappedUnderDefault"):
    val vectors = List(
      Nil,
      List("--jobs", "4"),
      List("-j", "4"),
      List("--jobs=7"),
      List("-j", "x"),
      List("-j"),
      List("-j", "1", "-j", "2"),
      List("--nope")
    )
    agree("mappedOverDefault", Scenarios.mappedOverDefault, C.mappedOverDefault, vectors)
    agree("mappedUnderDefault", Scenarios.mappedUnderDefault, C.mappedUnderDefault, vectors)

  test("consistency: repeatedOpt"):
    agree(
      "repeatedOpt",
      Scenarios.repeatedOpt,
      C.repeatedOpt,
      List(
        Nil,
        List("-I", "a", "--include", "b", "--include=c", "-s"),
        List("--include"),
        List("-I", "a", "-I", "a"),
        List("--strict", "--strict"),
        List("-I", "-s"),
        List("--include=", "-s"),
        List("-I", "a", "junk")
      )
    )

  test("consistency: mappedUnderRepeated"):
    agree(
      "mappedUnderRepeated",
      Scenarios.mappedUnderRepeated,
      C.mappedUnderRepeated,
      List(
        Nil,
        List("-n", "1", "-n", "2", "-n", "3"),
        List("--n=5"),
        List("-n", "two"),
        List("-n", "1", "-n", "two"),
        List("-n"),
        List("-n", "-2"),
        List("x")
      )
    )

  test("consistency: twoRequiredPositionals"):
    agree(
      "twoRequiredPositionals",
      Scenarios.twoRequiredPositionals,
      C.twoRequiredPositionals,
      List(
        Nil,
        List("a"),
        List("a", "b"),
        List("a", "b", "c"),
        List("--", "--src", "--dst"),
        List("--", "a", "--"),
        List("-", "-2"),
        List("--flag", "a"),
        List("a", "-x", "b"),
        List("--help"),
        List("a", "--help"),
        List("--", "--help", "b"),
        List("--", "--help", "b", "c")
      )
    )

  test("consistency: positionalWithDefault"):
    agree(
      "positionalWithDefault",
      Scenarios.positionalWithDefault,
      C.positionalWithDefault,
      List(
        Nil,
        List("f"),
        List("f", "3"),
        List("f", "xyz"),
        List("f", "3", "extra"),
        List("f", "-2"),
        List("--", "f", "3")
      )
    )

  test("consistency: positionalRepeatedLast"):
    agree(
      "positionalRepeatedLast",
      Scenarios.positionalRepeatedLast,
      C.positionalRepeatedLast,
      List(
        Nil,
        List("sh"),
        List("sh", "-", "b"),
        List("sh", "a", "b", "c", "d"),
        List("sh", "--", "-x", "-y"),
        List("--", "sh", "a"),
        List("-x", "sh"),
        List("--help"),
        List("sh", "--help"),
        List("sh", "--", "--help", "a")
      )
    )

  test("consistency: optionsAndPositionals"):
    agree(
      "optionsAndPositionals",
      Scenarios.optionsAndPositionals,
      C.optionsAndPositionals,
      List(
        Nil,
        List("T"),
        List("--jobs", "2", "T", "-v"),
        List("T", "--jobs=3"),
        List("-v", "T", "extra"),
        List("--jobs", "T"),
        List("--jobs"),
        List("-j", "1", "-j", "2", "T")
      )
    )

  test("consistency: numericPositionals"):
    agree(
      "numericPositionals",
      Scenarios.numericPositionals,
      C.numericPositionals,
      List(
        Nil,
        List("-", "-2"),
        List("x", "-5"),
        List("-o", "-3", "x"),
        List("x"),
        List("x", "y"),
        List("-o"),
        List("--offset=2", "x", "9"),
        List("x", "1", "2")
      )
    )

  test("consistency: withPure"):
    agree(
      "withPure",
      Scenarios.withPure,
      C.withPure,
      List(Nil, List("--on"), List("--on", "--on"), List("--off"), List("x"), List("--on=1"))
    )

  test("consistency: mappedCaseClass"):
    agree(
      "mappedCaseClass",
      Scenarios.mappedCaseClass,
      C.mappedCaseClass,
      List(
        Nil,
        List("T"),
        List("T", "-j", "2", "-v"),
        List("-v", "T"),
        List("T", "--jobs=x"),
        List("T", "U"),
        List("-j", "2")
      )
    )

  test("consistency: gitClone"):
    agree(
      "gitClone",
      Scenarios.gitClone,
      C.gitClone,
      List(
        Nil,
        List("clone", "repo"),
        List("clone", "repo", "--depth", "3", "-q"),
        List("clone", "repo", "--depth=x"),
        List("clone"),
        List("clone", "repo", "extra"),
        List("fetch"),
        List("-q", "clone", "repo"),
        List("--", "clone", "repo")
      )
    )

  test("consistency: gitRemote"):
    agree(
      "gitRemote",
      Scenarios.gitRemote,
      C.gitRemote,
      List(
        Nil,
        List("remote"),
        List("remote", "add", "o", "u"),
        List("remote", "add", "o"),
        List("remote", "remove", "o"),
        List("remote", "list"),
        List("remote", "list", "extra"),
        List("remote", "frob"),
        List("nope")
      )
    )

  test("consistency: git"):
    agree(
      "git",
      Scenarios.git,
      C.git,
      List(
        Nil,
        List("-v"),
        List("clone", "https://x", "-q"),
        List("clone", "repo", "--depth", "3"),
        List("-v", "remote", "add", "origin", "u"),
        List("remote", "remove", "origin"),
        List("remote", "list"),
        List("remote"),
        List("remote", "frob"),
        List("push"),
        List("clone", "repo", "--verbose"),
        List("clone", "repo", "-v"),
        List("--", "clone", "repo"),
        List("--verbose", "clone", "repo", "--depth"),
        List("-x", "clone"),
        List("--help"),
        List("-v", "--help"),
        List("clone", "--help"),
        List("clone", "repo", "--help"),
        List("remote", "--help"),
        List("remote", "add", "--help"),
        List("remote", "add", "o", "--help"),
        List("remote", "list", "--help"),
        List("--", "clone", "--help"),
        List("--help", "clone"),
        List("clone", "--help=x"),
        List("push", "--help")
      )
    )

  test("consistency: dockerRun and docker"):
    agree(
      "dockerRun",
      Scenarios.dockerRun,
      C.dockerRun,
      List(
        Nil,
        List("img"),
        List("-e", "A=1", "-e", "B=2", "--detach", "img", "c1", "c2"),
        List("--name=web", "img"),
        List("--name", "web", "img", "--", "-x"),
        List("-d"),
        List("--env"),
        List("img", "-d", "c1")
      )
    )
    agree(
      "docker",
      Scenarios.docker,
      C.docker,
      List(
        Nil,
        List("run"),
        List("run", "-e", "A=1", "-e", "B=2", "--detach", "img", "c1", "c2"),
        List("run", "--name=web", "img"),
        List("run", "-d"),
        List("run", "img", "--", "--flag"),
        List("build"),
        List("--", "run", "img"),
        List("--help"),
        List("run", "--help"),
        List("run", "--help=1"),
        List("run", "img", "--help"),
        List("run", "img", "--", "--help"),
        List("--", "run", "--help")
      )
    )

  test("consistency: unionResult"):
    agree(
      "unionResult",
      Scenarios.unionResult,
      C.unionResult,
      List(
        Nil,
        List("count", "-n", "5"),
        List("count", "--value=7"),
        List("count"),
        List("count", "-n", "x"),
        List("label", "hi"),
        List("label"),
        List("label", "a", "b"),
        List("other"),
        List("--help"),
        List("count", "--help"),
        List("label", "--help"),
        List("label", "hi", "--help")
      )
    )

  test("consistency: helpOptOut"):
    agree(
      "helpOptOut",
      Scenarios.helpOptOut,
      C.helpOptOut,
      List(
        Nil,
        List("--help"),
        List("version"),
        List("version", "--help"),
        List("version", "--help=x"),
        List("version", "--help", "--help"),
        List("version", "--", "--help"),
        List("version", "extra"),
        List("version", "--bogus", "--help"),
        List("build", "T"),
        List("build", "--help"),
        List("build", "--help=x"),
        List("build", "-f", "--help"),
        List("build", "--", "--help"),
        List("build"),
        List("--help", "version"),
        List("nope", "--help")
      )
    )

  test("consistency: nestedHelpOptOut"):
    agree(
      "nestedHelpOptOut",
      Scenarios.nestedHelpOptOut,
      C.nestedHelpOptOut,
      List(
        Nil,
        List("--help"),
        List("outer"),
        List("outer", "--help"),
        List("outer", "--help=x"),
        List("outer", "inner"),
        List("outer", "inner", "--help"),
        List("outer", "inner", "-d", "2"),
        List("outer", "inner", "--depth", "--help"),
        List("outer", "mute"),
        List("outer", "mute", "--help"),
        List("outer", "mute", "--help=x"),
        List("outer", "mute", "--loud"),
        List("outer", "mute", "--", "--help"),
        List("outer", "frob", "--help")
      )
    )

  // -------------------------------------------------------------------------------------------
  // Deterministic differential fuzz
  //
  // Same seed every run, so a failure is reproducible and the suite never flakes. Vectors are
  // short and drawn from a scenario-specific alphabet of real spellings mixed with junk, which
  // puts most of the weight on the error paths — exactly where two implementations drift apart.
  // -------------------------------------------------------------------------------------------

  private def lcg(seed: Int): () => Int =
    var state = seed
    () =>
      state = state * 1103515245 + 12345
      state & 0x7fffffff

  private def fuzz[R](
    name: String,
    cli: Cli[?, R],
    parser: Parser[R],
    alphabet: Vector[String],
    rounds: Int = 1000
  )(using munit.Location): Unit =
    val random = lcg(0x5eed_0001 ^ name.hashCode)
    var round = 0
    while round < rounds do
      val length = random() % 7
      val args = List.fill(length)(alphabet(random() % alphabet.length))
      assertEquals(
        parser.parse(args),
        Interp.parse(cli, args),
        s"$name disagreed on ${args.mkString("[", ", ", "]")}"
      )
      round += 1

  private val junk =
    Vector("-", "--", "-2", "-x", "--frob", "--frob=1", "-vq", "", "x", "=", "--=1", "--help",
      "--help=1")

  test("fuzz: flagsOnly"):
    fuzz(
      "flagsOnly",
      Scenarios.flagsOnly,
      C.flagsOnly,
      junk ++ Vector("--verbose", "-v", "--quiet", "-q", "--force", "--verbose=true", "--force=")
    )

  test("fuzz: typedOpts"):
    fuzz(
      "typedOpts",
      Scenarios.typedOpts,
      C.typedOpts,
      junk ++ Vector("--jobs", "-j", "--ratio", "-r", "--cache", "-c", "--seed", "--name", "-n",
        "4", "0.5", "true", "abc", "--jobs=4", "--cache=yes", "-j=4")
    )

  test("fuzz: defaultsAndOptional"):
    fuzz(
      "defaultsAndOptional",
      Scenarios.defaultsAndOptional,
      C.defaultsAndOptional,
      junk ++ Vector("--jobs", "-j", "--out", "-o", "--verbose", "-v", "8", "out", "--out=", "zz")
    )

  test("fuzz: repeatedOpt"):
    fuzz(
      "repeatedOpt",
      Scenarios.repeatedOpt,
      C.repeatedOpt,
      junk ++ Vector("--include", "-I", "--include=c", "-s", "--strict", "a", "b")
    )

  test("fuzz: numericPositionals"):
    fuzz(
      "numericPositionals",
      Scenarios.numericPositionals,
      C.numericPositionals,
      junk ++ Vector("--offset", "-o", "-3", "5", "x", "path", "--offset=7", "nope")
    )

  test("fuzz: positionalRepeatedLast"):
    fuzz(
      "positionalRepeatedLast",
      Scenarios.positionalRepeatedLast,
      C.positionalRepeatedLast,
      junk ++ Vector("sh", "a", "b", "c")
    )

  test("fuzz: mappedCaseClass"):
    fuzz(
      "mappedCaseClass",
      Scenarios.mappedCaseClass,
      C.mappedCaseClass,
      junk ++ Vector("T", "--jobs", "-j", "2", "-v", "--verbose", "--jobs=xyz")
    )

  test("fuzz: git"):
    fuzz(
      "git",
      Scenarios.git,
      C.git,
      junk ++ Vector("-v", "--verbose", "clone", "remote", "add", "remove", "list", "origin",
        "url", "--depth", "3", "-q", "--quiet", "repo")
    )

  test("fuzz: docker"):
    fuzz(
      "docker",
      Scenarios.docker,
      C.docker,
      junk ++ Vector("run", "-e", "--env", "A=1", "-d", "--detach", "--name", "--name=web", "img",
        "c1", "c2")
    )

  test("fuzz: unionResult"):
    fuzz(
      "unionResult",
      Scenarios.unionResult,
      C.unionResult,
      junk ++ Vector("count", "label", "-n", "--value", "5", "hi")
    )

  test("fuzz: helpOptOut"):
    fuzz(
      "helpOptOut",
      Scenarios.helpOptOut,
      C.helpOptOut,
      junk ++ Vector("version", "build", "T", "-f", "--fast")
    )

  test("fuzz: nestedHelpOptOut"):
    fuzz(
      "nestedHelpOptOut",
      Scenarios.nestedHelpOptOut,
      C.nestedHelpOptOut,
      junk ++ Vector("outer", "inner", "mute", "-d", "--depth", "2", "--loud")
    )
