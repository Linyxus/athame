package optparse

/** Type-level vocabulary describing the *shape* of a CLI grammar.
  *
  * These traits are never instantiated: they exist only as phantom type indices carried by
  * [[Cli]]. `C = Unit` in [[Shape.Flag]] / [[Shape.Opt]] means "no short name"; otherwise `C` is a
  * literal `Char` singleton. Names are literal `String` singletons.
  */
sealed trait Shape

object Shape:
  sealed trait Flag[N <: String, C <: Char | Unit] extends Shape
  sealed trait Opt[N <: String, C <: Char | Unit, A] extends Shape
  sealed trait Arg[N <: String, A] extends Shape
  sealed trait Both[L <: Shape, R <: Shape] extends Shape
  sealed trait OneOf[L <: Shape, R <: Shape] extends Shape
  sealed trait Sub[N <: String, S <: Shape] extends Shape
  sealed trait Mapped[S <: Shape] extends Shape
  sealed trait Default[S <: Shape] extends Shape
  sealed trait Repeated[S <: Shape] extends Shape
  sealed trait Pure extends Shape
