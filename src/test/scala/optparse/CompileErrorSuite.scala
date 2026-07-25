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
