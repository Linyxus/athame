package optparse

/** The carrier: a GADT indexed by the type-level [[Shape]] of the grammar and by the result type.
  *
  * Invariant in both parameters on purpose — variance and GADT refinement do not mix well here.
  * Users never call these constructors directly; they go through the surface API below.
  *
  * The value-level `short: Option[Char]` is deliberately not tied to the type-level `C`; the surface
  * constructors are what keep the two in sync. `Sub`'s `help: Boolean` mirrors its type-level `H`
  * the same way.
  */
enum Cli[S <: Shape, R]:
  case Flag[N <: String, C <: Char | Unit](name: N, short: Option[Char], desc: String)
    extends Cli[Shape.Flag[N, C], Boolean]
  case Opt[N <: String, C <: Char | Unit, A](name: N, short: Option[Char], desc: String, read: Read[A])
    extends Cli[Shape.Opt[N, C, A], A]
  case Arg[N <: String, A](name: N, desc: String, read: Read[A])
    extends Cli[Shape.Arg[N, A], A]
  case Both[S1 <: Shape, S2 <: Shape, A, B](l: Cli[S1, A], r: Cli[S2, B])
    extends Cli[Shape.Both[S1, S2], (A, B)]
  case OneOf[S1 <: Shape, S2 <: Shape, A, B](l: Cli[S1, A], r: Cli[S2, B])
    extends Cli[Shape.OneOf[S1, S2], A | B]
  case Sub[N <: String, S1 <: Shape, H <: Boolean, A](
    name: N,
    desc: String,
    inner: Cli[S1, A],
    help: Boolean
  ) extends Cli[Shape.Sub[N, S1, H], A]
  case Mapped[S1 <: Shape, A, B](inner: Cli[S1, A], f: A => B)
    extends Cli[Shape.Mapped[S1], B]
  case Default[S1 <: Shape, A](inner: Cli[S1, A], default: A)
    extends Cli[Shape.Default[S1], A]
  case Repeated[S1 <: Shape, A](inner: Cli[S1, A])
    extends Cli[Shape.Repeated[S1], List[A]]
  case Pure[A](value: A)
    extends Cli[Shape.Pure, A]

  // -------------------------------------------------------------------------------------------
  // Combinators.
  //
  // These are member methods, NOT extension methods, and that is load-bearing: an extension
  // method re-infers its own [S, A] for the receiver at every link of a combinator chain, and
  // when such a chain is elaborated under an outer call's unsolved type variables (e.g. as the
  // inline argument of `sub(...)`), the doubled type-variable pyramid sends the typer's
  // subtyping/lub machinery into divergence (observed: >120s for a 3-branch `|` chain inside
  // `sub`; 2s as members).
  // -------------------------------------------------------------------------------------------

  /** Conjunction; chains left-associatively into nested pairs. */
  def ~[S2 <: Shape, B](that: Cli[S2, B]): Cli[Shape.Both[S, S2], (R, B)] =
    Cli.Both[S, S2, R, B](this, that)

  /** Subcommand alternation; every branch must be subcommand-rooted (rule R2). */
  def |[S2 <: Shape, B](that: Cli[S2, B]): Cli[Shape.OneOf[S, S2], R | B] =
    Cli.OneOf[S, S2, R, B](this, that)

  def map[B](f: R => B): Cli[Shape.Mapped[S], B] =
    Cli.Mapped[S, R, B](this, f)

  def withDefault(default: R): Cli[Shape.Default[S], R] =
    Cli.Default[S, R](this, default)

  def optional: Cli[Shape.Default[Shape.Mapped[S]], Option[R]] =
    Cli.Default[Shape.Mapped[S], Option[R]](
      Cli.Mapped[S, R, Option[R]](this, (r: R) => Some(r)),
      None
    )

  def repeated: Cli[Shape.Repeated[S], List[R]] =
    Cli.Repeated[S, R](this)

// ---------------------------------------------------------------------------------------------
// Surface constructors
// ---------------------------------------------------------------------------------------------

/** A boolean flag with no short name: `--verbose`. */
def flag[N <: String & Singleton](name: N, desc: String): Cli[Shape.Flag[N, Unit], Boolean] =
  Cli.Flag[N, Unit](name, None, desc)

/** A boolean flag with a short name: `--verbose` / `-v`. */
def flag[N <: String & Singleton, C <: Char & Singleton](
  name: N,
  short: C,
  desc: String
): Cli[Shape.Flag[N, C], Boolean] =
  Cli.Flag[N, C](name, Some(short), desc)

/** A value-carrying option. The value type is explicit, the name is inferred as a singleton:
  * `opt[Int]("jobs", "desc")`. Scala 3 has no partial explicit type application, hence the builder.
  */
def opt[A]: OptBuilder[A] = OptBuilder.instance.asInstanceOf[OptBuilder[A]]

final class OptBuilder[A] private ():
  def apply[N <: String & Singleton](name: N, desc: String)(using
    read: Read[A]
  ): Cli[Shape.Opt[N, Unit, A], A] =
    Cli.Opt[N, Unit, A](name, None, desc, read)

  def apply[N <: String & Singleton, C <: Char & Singleton](name: N, short: C, desc: String)(using
    read: Read[A]
  ): Cli[Shape.Opt[N, C, A], A] =
    Cli.Opt[N, C, A](name, Some(short), desc, read)

object OptBuilder:
  private[optparse] val instance: OptBuilder[Any] = new OptBuilder[Any]

/** A positional argument: `arg[String]("target", "desc")`. */
def arg[A]: ArgBuilder[A] = ArgBuilder.instance.asInstanceOf[ArgBuilder[A]]

final class ArgBuilder[A] private ():
  def apply[N <: String & Singleton](name: N, desc: String)(using
    read: Read[A]
  ): Cli[Shape.Arg[N, A], A] =
    Cli.Arg[N, A](name, desc, read)

object ArgBuilder:
  private[optparse] val instance: ArgBuilder[Any] = new ArgBuilder[Any]

/** A named subcommand wrapping an inner scope, whose scope answers `--help` (A9.1). */
def sub[N <: String & Singleton, S <: Shape, A](
  name: N,
  desc: String,
  inner: Cli[S, A]
): Cli[Shape.Sub[N, S, true], A] =
  Cli.Sub[N, S, true, A](name, desc, inner, true)

/** A named subcommand that may opt out of the automatic `--help`: `sub(name, desc, inner, help =
  * false)`. The literal lands in the shape, so both backends decide statically whether that scope
  * intercepts the token. Opting out does not free the reserved name (R6/A9.3), and says nothing
  * about subcommands nested inside — help-ness is per scope (A9.2).
  */
def sub[N <: String & Singleton, S <: Shape, A, H <: Boolean & Singleton](
  name: N,
  desc: String,
  inner: Cli[S, A],
  help: H
): Cli[Shape.Sub[N, S, H], A] =
  Cli.Sub[N, S, H, A](name, desc, inner, help)

/** A constant that consumes no tokens. */
def pure[A](value: A): Cli[Shape.Pure, A] = Cli.Pure(value)

