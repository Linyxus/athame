package optparse

/** Decodes a command-line token into a value. `Left` carries a human-readable message that ends up
  * inside [[ParseError.InvalidValue]].
  */
trait Read[A]:
  def read(s: String): Either[String, A]

object Read:
  given readString: Read[String] = (s: String) => Right(s)

  given readInt: Read[Int] = (s: String) => s.toIntOption.toRight(s"expected an integer, got '$s'")

  given readLong: Read[Long] = (s: String) => s.toLongOption.toRight(s"expected a long, got '$s'")

  given readDouble: Read[Double] =
    (s: String) => s.toDoubleOption.toRight(s"expected a number, got '$s'")

  given readBoolean: Read[Boolean] = (s: String) =>
    s match
      case "true"  => Right(true)
      case "false" => Right(false)
      case other   => Left(s"expected 'true' or 'false', got '$other'")

  def apply[A](using r: Read[A]): Read[A] = r
