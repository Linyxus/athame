package optparse

/** Pins down the *exact* inferred types of the surface API from spec §3.
  *
  * `Wrap` captures the inferred type of its argument in a type member, so `=:=` below is a real
  * equality check rather than the conformance check a type ascription would give.
  */
final class Wrap[T](val value: T):
  type Out = T

def assertSameType[A, B](using @annotation.unused ev: A =:= B): Unit = ()

class InferenceSuite extends munit.FunSuite:

  val wFlag = Wrap(flag("verbose", "desc"))
  val wFlagShort = Wrap(flag("verbose", 'v', "desc"))
  val wOpt = Wrap(opt[Int]("jobs", "desc"))
  val wOptShort = Wrap(opt[Int]("jobs", 'j', "desc"))
  val wArg = Wrap(arg[String]("target", "desc"))
  val wPure = Wrap(pure(42))
  val wSub = Wrap(sub("build", "desc", flag("verbose", "desc")))

  val wBoth = Wrap(flag("verbose", 'v', "desc") ~ opt[Int]("jobs", "desc"))
  val wMapped = Wrap(opt[Int]("jobs", "desc").map(_.toString))
  val wDefault = Wrap(opt[Int]("jobs", "desc").withDefault(1))
  val wOptional = Wrap(opt[Int]("jobs", "desc").optional)
  val wRepeated = Wrap(opt[Int]("jobs", "desc").repeated)
  val wOneOf = Wrap(
    sub("a", "d", pure(1)) | sub("b", "d", pure("s"))
  )
  val wChained = Wrap(
    flag("a", "d") ~ flag("b", "d") ~ flag("c", "d")
  )

  test("flag without short") {
    assertSameType[wFlag.Out, Cli[Shape.Flag["verbose", Unit], Boolean]]
  }
  test("flag with short") {
    assertSameType[wFlagShort.Out, Cli[Shape.Flag["verbose", 'v'], Boolean]]
  }
  test("opt without short") {
    assertSameType[wOpt.Out, Cli[Shape.Opt["jobs", Unit, Int], Int]]
  }
  test("opt with short") {
    assertSameType[wOptShort.Out, Cli[Shape.Opt["jobs", 'j', Int], Int]]
  }
  test("arg") {
    assertSameType[wArg.Out, Cli[Shape.Arg["target", String], String]]
  }
  test("pure") {
    assertSameType[wPure.Out, Cli[Shape.Pure, Int]]
  }
  test("sub") {
    assertSameType[wSub.Out, Cli[Shape.Sub["build", Shape.Flag["verbose", Unit]], Boolean]]
  }
  test("both") {
    assertSameType[
      wBoth.Out,
      Cli[Shape.Both[Shape.Flag["verbose", 'v'], Shape.Opt["jobs", Unit, Int]], (Boolean, Int)]
    ]
  }
  test("chained both nests to the left") {
    assertSameType[
      wChained.Out,
      Cli[
        Shape.Both[
          Shape.Both[Shape.Flag["a", Unit], Shape.Flag["b", Unit]],
          Shape.Flag["c", Unit]
        ],
        ((Boolean, Boolean), Boolean)
      ]
    ]
  }
  test("mapped") {
    assertSameType[wMapped.Out, Cli[Shape.Mapped[Shape.Opt["jobs", Unit, Int]], String]]
  }
  test("withDefault") {
    assertSameType[wDefault.Out, Cli[Shape.Default[Shape.Opt["jobs", Unit, Int]], Int]]
  }
  test("optional") {
    assertSameType[
      wOptional.Out,
      Cli[Shape.Default[Shape.Mapped[Shape.Opt["jobs", Unit, Int]]], Option[Int]]
    ]
  }
  test("repeated") {
    assertSameType[wRepeated.Out, Cli[Shape.Repeated[Shape.Opt["jobs", Unit, Int]], List[Int]]]
  }
  test("oneOf") {
    assertSameType[
      wOneOf.Out,
      Cli[Shape.OneOf[Shape.Sub["a", Shape.Pure], Shape.Sub["b", Shape.Pure]], Int | String]
    ]
  }
