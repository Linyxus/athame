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
  *
  * [[UnknownSubcommand]] and [[MissingSubcommand]] carry a rendering as well (A10.1), but of the
  * scope that owns the subcommand group they were raised in: `tool sync` with no subcommand is a
  * question about `sync`, so the answer is sync's help rather than the root's. That text is
  * byte-identical to what `--help` in the same scope returns. A scope that opted out of `--help`
  * (A9.1) still supplies its help here, rendered without the row advertising an option it does not
  * answer to (A10.2) — so the field is always populated, for every scope.
  *
  * [[InvalidValue]]'s third field is `detail` rather than `message` (A10.6) because [[message]]
  * below is the whole one-line sentence for every case, and this is only the fragment a [[Read]]
  * contributed to it.
  */
enum ParseError:
  case HelpRequested(help: String)
  case UnknownOption(name: String)
  case MissingOption(name: String)
  case MissingValue(name: String)
  case DuplicateOption(name: String)
  case InvalidValue(name: String, value: String, detail: String)
  case MissingArgument(name: String)
  case UnexpectedArgument(value: String)
  case UnknownSubcommand(value: String, expected: List[String], help: String)
  case MissingSubcommand(expected: List[String], help: String)

  /** One line, in the terms the user typed, with no help text in it (A10.4).
    *
    * The expected subcommand names are deliberately left out of the two subcommand messages: what
    * follows them is [[contextHelp]], whose Commands section already lists every name with its
    * description.
    */
  def message: String =
    this match
      case HelpRequested(_)                  => "help requested"
      case UnknownOption(name)               => s"unknown option '$name'"
      case MissingOption(name)               => s"missing required option '--$name'"
      case MissingValue(name)                => s"option '--$name' requires a value"
      case DuplicateOption(name)             => s"option '--$name' may only be given once"
      case InvalidValue(name, value, detail) => s"invalid value '$value' for '$name': $detail"
      case MissingArgument(name)             => s"missing argument '$name'"
      case UnexpectedArgument(value)         => s"unexpected argument '$value'"
      case UnknownSubcommand(value, _, _)    => s"unknown command '$value'"
      case MissingSubcommand(_, _)           => "missing command"

  /** The help to print beside [[message]] when the error knows which scope it came from. `None`
    * leaves the choice to the caller, which typically falls back to [[Parser.help]].
    */
  def contextHelp: Option[String] =
    this match
      case UnknownSubcommand(_, _, help) => Some(help)
      case MissingSubcommand(_, help)    => Some(help)
      case _                             => None

trait Parser[R]:
  def parse(args: Seq[String]): Either[ParseError, R]
  def help: String
