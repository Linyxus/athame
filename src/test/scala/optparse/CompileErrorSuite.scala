package optparse

/** The compile-time half of the contract: trees the interpreter rejects with an
  * `IllegalArgumentException` must be rejected by `compile` before the program ever runs.
  *
  * Each snippet is type-checked (and macro-expanded) by `compileErrors`, so what is asserted here
  * is the text the user sees in their build. Every message is prefixed `optparse:` and names the
  * offender.
  */
class CompileErrorSuite extends munit.FunSuite:

  private def assertRejected(errors: String, offender: String)(using munit.Location): Unit =
    assert(errors.nonEmpty, "expected a compile error, but the snippet compiled")
    assert(errors.contains("optparse:"), errors)
    assert(errors.contains(offender), errors)

  // -------------------------------------------------------------------------------------------
  // R1 — unique option names within a scope
  // -------------------------------------------------------------------------------------------

  test("R1: duplicate long option name"):
    assertRejected(
      compileErrors("""compile(flag("x", "d") ~ opt[Int]("x", "d"))"""),
      "--x"
    )

  test("R1: duplicate short option name"):
    assertRejected(
      compileErrors("""compile(flag("aa", 'x', "d") ~ flag("bb", 'x', "d"))"""),
      "-x"
    )

  test("R1: duplicate long name inside a subcommand scope"):
    assertRejected(
      compileErrors("""compile(sub("go", "d", flag("x", "d") ~ flag("x", "d")))"""),
      "--x"
    )

  // -------------------------------------------------------------------------------------------
  // R2 — subcommand groups
  // -------------------------------------------------------------------------------------------

  test("R2: duplicate subcommand name in a group"):
    assertRejected(
      compileErrors("""compile(sub("a", "d", pure(1)) | sub("a", "d", pure(2)))"""),
      "duplicate subcommand name 'a'"
    )

  test("R2: a OneOf branch must be subcommand-rooted"):
    assertRejected(
      compileErrors("""compile(flag("x", "d") | sub("b", "d", pure(1)))"""),
      "subcommand-rooted"
    )

  // -------------------------------------------------------------------------------------------
  // R3 — one subcommand group per scope, never beside positionals
  // -------------------------------------------------------------------------------------------

  test("R3: positionals may not share a scope with a subcommand group"):
    assertRejected(
      compileErrors("""compile(arg[String]("target", "d") ~ sub("a", "d", pure(1)))"""),
      "'target'"
    )

  test("R3: at most one subcommand group per scope"):
    assertRejected(
      compileErrors("""compile(sub("a", "d", pure(1)) ~ sub("b", "d", pure(2)))"""),
      "at most one subcommand group"
    )

  // -------------------------------------------------------------------------------------------
  // R4 — what Default and Repeated may wrap
  // -------------------------------------------------------------------------------------------

  test("R4: Default may not wrap a Flag"):
    assertRejected(
      compileErrors("""compile(flag("x", "d").withDefault(true))"""),
      "--x: Default may not wrap a Flag"
    )

  test("R4: Repeated may not wrap a Flag"):
    assertRejected(
      compileErrors("""compile(flag("x", "d").repeated)"""),
      "--x: Repeated may not wrap a Flag"
    )

  test("R4: Repeated and Default may not stack"):
    assertRejected(
      compileErrors("""compile(opt[Int]("x", "d").withDefault(1).repeated)"""),
      "stacked"
    )

  test("R4: Default may not wrap a composite"):
    assertRejected(
      compileErrors("""compile((flag("x", "d") ~ flag("y", "d")).withDefault((true, true)))"""),
      "Both"
    )

  // -------------------------------------------------------------------------------------------
  // R5 — positional ordering
  // -------------------------------------------------------------------------------------------

  test("R5: a repeated positional must be last"):
    assertRejected(
      compileErrors("""compile(arg[String]("a", "d").repeated ~ arg[String]("b", "d"))"""),
      "repeated positional 'a' must be last"
    )

  test("R5: at most one repeated positional"):
    assertRejected(
      compileErrors("""compile(arg[String]("a", "d").repeated ~ arg[String]("b", "d").repeated)"""),
      "at most one repeated positional"
    )

  test("R5: no required positional after a defaulted one"):
    assertRejected(
      compileErrors("""compile(arg[String]("a", "d").withDefault("x") ~ arg[String]("b", "d"))"""),
      "may not follow defaulted positional 'a'"
    )

  // -------------------------------------------------------------------------------------------
  // R6 — `help` is reserved for the automatic option (A8.4)
  // -------------------------------------------------------------------------------------------

  private def assertAccepted(errors: String)(using munit.Location): Unit =
    assertEquals(errors, "", s"expected the snippet to compile, got: $errors")

  test("R6: a Flag may not be named help"):
    assertRejected(
      compileErrors("""compile(flag("help", 'h', "d"))"""),
      "'help' is reserved for the automatic help option"
    )

  test("R6: an Opt may not be named help"):
    assertRejected(
      compileErrors("""compile(opt[Int]("help", "d"))"""),
      "'help' is reserved for the automatic help option"
    )

  test("R6: the reservation reaches subcommand scopes too"):
    assertRejected(
      compileErrors("""compile(sub("go", "d", flag("help", "d")))"""),
      "'help' is reserved"
    )

  test("R6 (A9.3): the reservation holds inside a help-disabled scope too"):
    assertRejected(
      compileErrors("""compile(sub("quiet", "d", flag("help", "d"), help = false))"""),
      "'help' is reserved for the automatic help option"
    )

  test("R6 (A9.3): an Opt named help is rejected inside a help-disabled scope"):
    assertRejected(
      compileErrors("""compile(sub("quiet", "d", opt[Int]("help", "d"), help = false))"""),
      "'help' is reserved"
    )

  test("R6 (A4): an Arg named help is a legal metavar"):
    assertAccepted(compileErrors("""compile(arg[String]("help", "d"))"""))

  test("R6: a subcommand named help is legal"):
    assertAccepted(compileErrors("""compile(sub("help", "d", pure(1)))"""))

  test("R6: short 'h' is not reserved"):
    assertAccepted(compileErrors("""compile(flag("hop", 'h', "d"))"""))

  // -------------------------------------------------------------------------------------------
  // Shapes that are not statically known
  // -------------------------------------------------------------------------------------------

  test("widened shape: a type ascription that erases the shape"):
    assertRejected(
      compileErrors("""
        val widened: Cli[?, Int] = pure(1)
        compile(widened)
      """),
      "statically known"
    )

  test("widened shape: an abstract shape parameter"):
    assertRejected(
      compileErrors("""
        def build[S <: Shape](cli: Cli[S, Int]): Parser[Int] = compile(cli)
      """),
      "statically known"
    )

  test("non-literal option name"):
    assertRejected(
      compileErrors("""
        val name: String = "dynamic"
        compile(flag(name, "d"))
      """),
      "statically known"
    )

  test("non-literal help flag (A9.6)"):
    assertRejected(
      compileErrors("""
        val b: Boolean = false
        compile(sub("x", "d", pure(1), help = b))
      """),
      "statically known"
    )

  test("a literal help flag compiles in both polarities (A9.6)"):
    assertAccepted(compileErrors("""compile(sub("x", "d", pure(1), help = false))"""))
    assertAccepted(compileErrors("""compile(sub("x", "d", pure(1), help = true))"""))
