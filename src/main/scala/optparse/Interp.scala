package optparse

import scala.collection.mutable.ListBuffer

/** Runtime reference interpreter for [[Cli]].
  *
  * Each command scope is prepared in one pass: [[build]] registers the mutable slots used by the
  * token scanner and, at the same time, returns the thunk that reconstructs that scope's result.
  * Keeping those two jobs in the same traversal is important: slot order and result order cannot
  * accidentally diverge.
  */
object Interp:
  private type Builder = () => Either[ParseError, Any]
  private type ErasedMap = Any => Any
  private type ErasedRead = String => Either[String, Any]

  private enum Arity:
    case Required
    case Defaulted(value: Any)
    case Repeated

  private sealed trait OptionSlot:
    def longName: String
    def short: Option[Char]

  private final class FlagSlot(val longName: String, val short: Option[Char]) extends OptionSlot:
    private var present = false

    def markPresent(): Unit = present = true
    def result: Either[ParseError, Any] = Right(present)

  private final class OptSlot(
    val longName: String,
    val short: Option[Char],
    read: ErasedRead,
    transform: ErasedMap,
    arity: Arity
  ) extends OptionSlot:
    private val values = ListBuffer.empty[Any]

    def add(raw: String): Either[ParseError, Unit] =
      arity match
        case Arity.Repeated => decode(raw).map(values += _).map(_ => ())
        case _ if values.nonEmpty => Left(ParseError.DuplicateOption(longName))
        case _ => decode(raw).map(values += _).map(_ => ())

    def result: Either[ParseError, Any] =
      arity match
        case Arity.Required =>
          values.headOption.toRight(ParseError.MissingOption(longName))
        case Arity.Defaulted(default) =>
          Right(values.headOption.getOrElse(default))
        case Arity.Repeated =>
          Right(values.toList)

    private def decode(raw: String): Either[ParseError, Any] =
      read(raw) match
        case Right(value) => Right(transform(value))
        case Left(message) =>
          Left(ParseError.InvalidValue(longName, raw, message))

  private final class PositionalSlot(
    val name: String,
    read: ErasedRead,
    transform: ErasedMap,
    val arity: Arity
  ):
    private val values = ListBuffer.empty[Any]

    def isRepeated: Boolean = arity == Arity.Repeated

    def add(raw: String): Either[ParseError, Unit] =
      read(raw) match
        case Right(value) =>
          values += transform(value)
          Right(())
        case Left(message) =>
          Left(ParseError.InvalidValue(name, raw, message))

    def result: Either[ParseError, Any] =
      arity match
        case Arity.Required =>
          values.headOption.toRight(ParseError.MissingArgument(name))
        case Arity.Defaulted(default) =>
          Right(values.headOption.getOrElse(default))
        case Arity.Repeated =>
          Right(values.toList)

  private final case class RawSubEntry(
    name: String,
    desc: String,
    inner: Cli[?, ?],
    help: Boolean,
    wrap: ErasedMap
  )

  private final case class SubEntry(
    name: String,
    desc: String,
    program: Program,
    wrap: ErasedMap
  )

  private final class SubgroupSlot(val rawEntries: List[RawSubEntry]):
    private var parsed: Option[Either[ParseError, Any]] = None
    private var preparedEntries: List[SubEntry] = Nil

    def expectedNames: List[String] = rawEntries.map(_.name)
    def entries: List[SubEntry] = preparedEntries

    def prepareEntries(): Unit =
      preparedEntries = rawEntries.map { entry =>
        SubEntry(entry.name, entry.desc, prepareScope(entry.inner, entry.help), entry.wrap)
      }

    def select(result: Either[ParseError, Any]): Unit = parsed = Some(result)

    def result: Either[ParseError, Any] =
      parsed.getOrElse(Left(ParseError.MissingSubcommand(expectedNames)))

  private final class Scope:
    val options = ListBuffer.empty[OptionSlot]
    val positionals = ListBuffer.empty[PositionalSlot]
    val subgroups = ListBuffer.empty[SubgroupSlot]

    def registerOption(slot: OptionSlot): Unit =
      // R6/A8.4: the scanner answers `--help` in every scope, so a user option cannot claim the
      // name. Short 'h' stays free, and Arg/Sub names are unaffected. A9.3: the reservation is
      // universal — a scope that opted out of auto-help still may not define its own `--help`, so
      // that re-enabling it later cannot break a working grammar.
      if slot.longName == Help.reservedOptionName then
        violation(
          s"option name '${Help.reservedOptionName}' is reserved for the automatic help option"
        )

      if options.exists(_.longName == slot.longName) then
        violation(s"duplicate long option name '--${slot.longName}' in scope")

      slot.short.foreach { short =>
        if options.exists(_.short.contains(short)) then
          violation(s"duplicate short option name '-$short' in scope")
      }

      options += slot

    def validateLocalStructure(): Unit =
      if subgroups.size > 1 then
        violation("a scope may contain at most one subcommand group")

      if subgroups.nonEmpty && positionals.nonEmpty then
        violation(
          s"positional argument '${positionals.head.name}' may not be used in a scope with a subcommand group"
        )

      val repeated = positionals.filter(_.isRepeated).toList
      if repeated.size > 1 then
        violation(
          s"at most one repeated positional is allowed in a scope; '${repeated(1).name}' is also repeated"
        )

      repeated.headOption.foreach { slot =>
        if !positionals.lastOption.contains(slot) then
          violation(s"repeated positional '${slot.name}' must be last")
      }

      var firstDefaulted: Option[String] = None
      positionals.foreach { slot =>
        slot.arity match
          case Arity.Defaulted(_) =>
            if firstDefaulted.isEmpty then firstDefaulted = Some(slot.name)
          case Arity.Required =>
            firstDefaulted.foreach { previous =>
              violation(
                s"required positional '${slot.name}' may not follow defaulted positional '$previous'"
              )
            }
          case Arity.Repeated => ()
      }

  /** `cli` is kept only so that a `--help` token can render this scope's own grammar (A8.1);
    * `helpEnabled` is that scope's `H` (A9.1), false for a subcommand declared with `help = false`.
    */
  private final case class Program(
    cli: Cli[?, ?],
    helpEnabled: Boolean,
    scope: Scope,
    builder: Builder
  )

  private enum Leaf:
    case OptLeaf(name: String, short: Option[Char], read: ErasedRead)
    case ArgLeaf(name: String, read: ErasedRead)

  private final case class WrappedLeaf(leaf: Leaf, transform: ErasedMap)

  /** Parse one argument vector. Structural validation is eager because preparation recursively
    * visits every subcommand scope before token scanning starts.
    */
  def parse[R](cli: Cli[?, R], args: Seq[String]): Either[ParseError, R] =
    // A9.1: the root scope always auto-accepts `--help`; only subcommands may opt out.
    val program = prepareScope(cli, helpEnabled = true)
    parseScope(program, args).map(_.asInstanceOf[R])

  private def prepareScope(root: Cli[?, ?], helpEnabled: Boolean): Program =
    val scope = new Scope
    val builder = build(root, scope)
    scope.validateLocalStructure()
    scope.subgroups.foreach(_.prepareEntries())
    Program(root, helpEnabled, scope, builder)

  /** The mandated single-pass slot collection/result reconstruction traversal for one scope. */
  private def build(node: Cli[?, ?], scope: Scope): Builder =
    node match
      case Cli.Flag(name, short, _) =>
        val slot = new FlagSlot(name, short)
        scope.registerOption(slot)
        () => slot.result

      case Cli.Opt(name, short, _, read) =>
        val slot = new OptSlot(name, short, eraseRead(read), identityMap, Arity.Required)
        scope.registerOption(slot)
        () => slot.result

      case Cli.Arg(name, _, read) =>
        val slot = new PositionalSlot(name, eraseRead(read), identityMap, Arity.Required)
        scope.positionals += slot
        () => slot.result

      case Cli.Both(left, right) =>
        val buildLeft = build(left, scope)
        val buildRight = build(right, scope)
        () =>
          for
            leftValue <- buildLeft()
            rightValue <- buildRight()
          yield (leftValue, rightValue)

      case Cli.Mapped(inner, f) =>
        val buildInner = build(inner, scope)
        val map = eraseMap(f)
        () => buildInner().map(map)

      case Cli.Default(inner, default) =>
        val wrapped = unwrapAnnotated(inner, "Default")
        wrapped.leaf match
          case Leaf.OptLeaf(name, short, read) =>
            val slot =
              new OptSlot(name, short, read, wrapped.transform, Arity.Defaulted(default))
            scope.registerOption(slot)
            () => slot.result
          case Leaf.ArgLeaf(name, read) =>
            val slot =
              new PositionalSlot(name, read, wrapped.transform, Arity.Defaulted(default))
            scope.positionals += slot
            () => slot.result

      case Cli.Repeated(inner) =>
        val wrapped = unwrapAnnotated(inner, "Repeated")
        wrapped.leaf match
          case Leaf.OptLeaf(name, short, read) =>
            val slot = new OptSlot(name, short, read, wrapped.transform, Arity.Repeated)
            scope.registerOption(slot)
            () => slot.result
          case Leaf.ArgLeaf(name, read) =>
            val slot = new PositionalSlot(name, read, wrapped.transform, Arity.Repeated)
            scope.positionals += slot
            () => slot.result

      case Cli.Pure(value) =>
        () => Right(value)

      case Cli.Sub(_, _, _, _) =>
        buildSubgroup(node, scope)

      case Cli.OneOf(_, _) =>
        buildSubgroup(node, scope)

  private def buildSubgroup(node: Cli[?, ?], scope: Scope): Builder =
    val entries = collectSubs(node, identityMap)
    val seenNames = ListBuffer.empty[String]
    entries.foreach { entry =>
      if seenNames.contains(entry.name) then
        violation(s"duplicate subcommand name '${entry.name}' in group")
      seenNames += entry.name
    }

    val slot = new SubgroupSlot(entries)
    scope.subgroups += slot
    () => slot.result

  /** Unwrap maps below a Default/Repeated. The composed map is run once per supplied occurrence;
    * defaults themselves are already values of the annotated node's result type and are untouched.
    */
  private def unwrapAnnotated(node: Cli[?, ?], annotation: String): WrappedLeaf =
    def loop(current: Cli[?, ?], wrap: ErasedMap): WrappedLeaf =
      current match
        case Cli.Mapped(inner, f) =>
          val map = eraseMap(f)
          loop(inner, value => wrap(map(value)))
        case Cli.Opt(name, short, _, read) =>
          WrappedLeaf(Leaf.OptLeaf(name, short, eraseRead(read)), wrap)
        case Cli.Arg(name, _, read) =>
          WrappedLeaf(Leaf.ArgLeaf(name, eraseRead(read)), wrap)
        case Cli.Flag(name, _, _) =>
          violation(s"--$name: $annotation may not wrap a Flag")
        case Cli.Default(_, _) =>
          violation(s"$annotation may not be stacked with Default")
        case Cli.Repeated(_) =>
          violation(s"$annotation may not be stacked with Repeated")
        case other =>
          violation(
            s"$annotation may wrap only an Opt or Arg (found ${nodeKind(other)})"
          )

    loop(node, identityMap)

  /** Collect one subcommand group. Maps inside a OneOf branch are branch-specific wrappers; a map
    * above the whole group is handled by [[build]] like any other result map.
    */
  private def collectSubs(node: Cli[?, ?], wrap: ErasedMap): List[RawSubEntry] =
    node match
      case Cli.Sub(name, desc, inner, help) =>
        List(RawSubEntry(name, desc, inner, help, wrap))
      case Cli.OneOf(left, right) =>
        collectSubs(left, wrap) ++ collectSubs(right, wrap)
      case Cli.Mapped(inner, f) =>
        val map = eraseMap(f)
        collectSubs(inner, value => wrap(map(value)))
      case other =>
        violation(
          s"OneOf branch must be subcommand-rooted (found ${nodeKind(other)})"
        )

  private def parseScope(program: Program, args: Seq[String]): Either[ParseError, Any] =
    val scope = program.scope
    var i = 0
    var positionalIndex = 0
    var afterDoubleDash = false
    var scanning = true

    while i < args.length && scanning do
      val token = args(i)

      // Only the first `--` terminates option parsing; later ones are ordinary positionals.
      if !afterDoubleDash && token == "--" then
        afterDoubleDash = true
        i += 1
      // A post-`--` token still dispatches a subcommand: dispatch keys off the first
      // positional-looking token, and `--` only forces tokens to look positional.
      else if afterDoubleDash || isPositionalLike(token) then
        scope.subgroups.headOption match
          case Some(group) =>
            group.entries.find(_.name == token) match
              case None =>
                return Left(ParseError.UnknownSubcommand(token, group.expectedNames))
              case Some(entry) =>
                val innerResult = parseScope(entry.program, args.drop(i + 1)).map(entry.wrap)
                group.select(innerResult)
                scanning = false
          case None =>
            consumePositional(scope, positionalIndex, token) match
              case Left(error) => return Left(error)
              case Right(repeated) =>
                if !repeated then positionalIndex += 1
                i += 1
      else if token.startsWith("--") then
        val equalsAt = token.indexOf('=')
        val name =
          if equalsAt < 0 then token.substring(2)
          else token.substring(2, equalsAt)
        val inlineValue =
          if equalsAt < 0 then None
          else Some(token.substring(equalsAt + 1))

        // A8.1/A8.7: help is a scanner-level slot rather than a user flag, so it is answered before
        // the option table is consulted and it short-circuits the scan — which is what makes it beat
        // every missing-check (A8.3). It still behaves like a flag for `=value` (A8.2). A9.2: in a
        // help-disabled scope there is no interception at all, so the token falls through to the
        // option table and comes back out as an ordinary UnknownOption, `=value` included.
        if program.helpEnabled && name == Help.reservedOptionName then
          inlineValue match
            case Some(value) =>
              return Left(ParseError.InvalidValue(name, value, "flag does not take a value"))
            case None =>
              return Left(
                ParseError.HelpRequested(Help.render(program.cli, program.helpEnabled))
              )

        scope.options.find(_.longName == name) match
          case None =>
            // Ruling A2: an unknown option is reported exactly as the user wrote it.
            return Left(ParseError.UnknownOption(token))
          case Some(flag: FlagSlot) =>
            // Ruling A1: the option is known, so say what is actually wrong with it.
            inlineValue match
              case Some(value) =>
                return Left(ParseError.InvalidValue(name, value, "flag does not take a value"))
              case None =>
                flag.markPresent()
                i += 1
          case Some(option: OptSlot) =>
            inlineValue match
              case Some(value) =>
                option.add(value) match
                  case Left(error) => return Left(error)
                  case Right(_)    => i += 1
              case None =>
                if i + 1 >= args.length then
                  return Left(ParseError.MissingValue(option.longName))
                val value = args(i + 1)
                option.add(value) match
                  case Left(error) => return Left(error)
                  case Right(_)    => i += 2
      else if token.length == 2 then
        val short = token.charAt(1)
        scope.options.find(_.short.contains(short)) match
          case None =>
            return Left(ParseError.UnknownOption(token))
          case Some(flag: FlagSlot) =>
            flag.markPresent()
            i += 1
          case Some(option: OptSlot) =>
            if i + 1 >= args.length then
              return Left(ParseError.MissingValue(option.longName))
            val value = args(i + 1)
            option.add(value) match
              case Left(error) => return Left(error)
              case Right(_)    => i += 2
      else
        return Left(ParseError.UnknownOption(token))

    program.builder()

  /** Returns whether the consumed slot is repeated (and should therefore remain current). */
  private def consumePositional(
    scope: Scope,
    positionalIndex: Int,
    token: String
  ): Either[ParseError, Boolean] =
    if positionalIndex >= scope.positionals.length then
      Left(ParseError.UnexpectedArgument(token))
    else
      val slot = scope.positionals(positionalIndex)
      slot.add(token).map(_ => slot.isRepeated)

  private def isPositionalLike(token: String): Boolean =
    !token.startsWith("-") ||
      token == "-" ||
      (token.length >= 2 && token.charAt(1).isDigit)

  private def eraseRead[A](read: Read[A]): ErasedRead =
    (value: String) => read.read(value)

  private def eraseMap[A, B](f: A => B): ErasedMap =
    (value: Any) => f(value.asInstanceOf[A])

  private val identityMap: ErasedMap = (value: Any) => value

  private def nodeKind(node: Cli[?, ?]): String =
    node match
      case Cli.Flag(_, _, _)    => "Flag"
      case Cli.Opt(_, _, _, _)  => "Opt"
      case Cli.Arg(_, _, _)     => "Arg"
      case Cli.Both(_, _)       => "Both"
      case Cli.OneOf(_, _)      => "OneOf"
      case Cli.Sub(_, _, _, _)  => "Sub"
      case Cli.Mapped(_, _)     => "Mapped"
      case Cli.Default(_, _)    => "Default"
      case Cli.Repeated(_)      => "Repeated"
      case Cli.Pure(_)          => "Pure"

  private def violation(message: String): Nothing =
    throw new IllegalArgumentException(s"optparse: $message")
