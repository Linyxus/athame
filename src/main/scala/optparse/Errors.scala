package optparse

/** Failures produced while parsing an argument vector.
  *
  * Naming convention: [[UnknownOption]] carries the token *as written* (dashes included); every
  * other case that names an option carries its bare long name (no dashes), even when the user wrote
  * the short form.
  *
  * [[HelpRequested]] is a `Left` without being a failure: `--help` short-circuits the scan and
  * hands back the rendered help for the scope that saw the token, so callers report it and exit
  * successfully rather than complaining.
  */
enum ParseError:
  case HelpRequested(help: String)
  case UnknownOption(name: String)
  case MissingOption(name: String)
  case MissingValue(name: String)
  case DuplicateOption(name: String)
  case InvalidValue(name: String, value: String, message: String)
  case MissingArgument(name: String)
  case UnexpectedArgument(value: String)
  case UnknownSubcommand(value: String, expected: List[String])
  case MissingSubcommand(expected: List[String])

trait Parser[R]:
  def parse(args: Seq[String]): Either[ParseError, R]
  def help: String
