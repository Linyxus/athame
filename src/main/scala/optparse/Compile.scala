package optparse

import scala.quoted.*
import scala.collection.mutable.ListBuffer

/** Compile a [[Cli]] description into a specialized [[Parser]].
  *
  * The whole grammar is read off the phantom shape type `S`, never off the term tree of `cli`, so
  * the CLI may be built anywhere — as long as its shape survives inference. Structural rules R1-R5
  * are checked here, at compile time; the generated parser is a straight-line token scanner that
  * never looks at the shape again.
  *
  * The one thing that will make this fail is a widened type: `val c: Cli[?, Int] = ...` throws the
  * shape away and there is nothing left to compile.
  */
inline def compile[S <: Shape, R](cli: Cli[S, R]): Parser[R] =
  ${ CompileMacro.compileImpl[S, R]('cli) }

/** Runtime support for macro-generated parsers. Public because generated code lives in whatever
  * package the user compiles it in; not intended to be called by hand.
  */
object ParserRuntime:

  /** One option or positional slot. Which fields matter is decided at compile time: flags and
    * single-valued slots use `set`/`value`, repeated ones accumulate into `acc` in reverse.
    */
  final class Slot:
    var set: Boolean = false
    var value: Any = null
    var acc: List[Any] = Nil

  /** The mutable state of one scope's token scan. Generated code only ever calls these methods, so
    * the scan loop stays readable in the expansion and no assignment has to survive a splice.
    */
  final class Scan(val args: Seq[String], slotCount: Int):
    private val slots: Array[Slot] = Array.fill(slotCount)(new Slot)
    private var cursor: Int = 0
    private var positional: Int = 0
    private var doubleDash: Boolean = false
    private var running: Boolean = true
    private var error: ParseError = null
    private var subResult: Either[ParseError, Any] = null

    def ok: Boolean = error == null
    def failed: Boolean = error != null
    def failure: ParseError = error
    def fail(e: ParseError): Unit = error = e

    def exhausted: Boolean = cursor >= args.length
    def token: String = args(cursor)
    def advance(by: Int): Unit = cursor += by
    def hasNextToken: Boolean = cursor + 1 < args.length
    def nextToken: String = args(cursor + 1)
    def rest: Seq[String] = args.drop(cursor + 1)

    def scanning: Boolean = running
    def terminated: Boolean = doubleDash
    def terminate(): Unit =
      doubleDash = true
      cursor += 1

    def positionalIndex: Int = positional
    def advancePositional(): Unit = positional += 1

    def flagSeen(slot: Int): Unit =
      slots(slot).set = true
      cursor += 1

    def isSet(slot: Int): Boolean = slots(slot).set
    def valueOf(slot: Int): Any = slots(slot).value

    def store(slot: Int, value: Any): Unit =
      val cell = slots(slot)
      cell.value = value
      cell.set = true

    def append(slot: Int, value: Any): Unit =
      val cell = slots(slot)
      cell.acc = value :: cell.acc

    def collected(slot: Int): List[Any] = slots(slot).acc.reverse

    def dispatched(result: Either[ParseError, Any]): Unit =
      subResult = result
      running = false

    def hasSub: Boolean = subResult != null
    def sub: Either[ParseError, Any] = subResult

  /** A token is positional-looking when it does not start with `-`, is exactly `-`, or is a
    * negative number such as `-2`.
    */
  def isPositionalLike(token: String): Boolean =
    !token.startsWith("-") ||
      token == "-" ||
      (token.length >= 2 && token.charAt(1).isDigit)

  /** Payload-table accessors. The macro guarantees the index holds the right kind of payload. */
  def read(payload: Any, raw: String): Either[String, Any] =
    payload.asInstanceOf[Read[Any]].read(raw)

  def transform(payload: Any, value: Any): Any =
    payload.asInstanceOf[Any => Any](value)

  final class CompiledParser[R](cli: Cli[?, ?], run: Seq[String] => Either[ParseError, Any])
      extends Parser[R]:
    def parse(args: Seq[String]): Either[ParseError, R] =
      run(args).asInstanceOf[Either[ParseError, R]]
    def help: String = Help.render(cli)

/** Implementation of [[compile]]. Public only so that inlining works from any package. */
object CompileMacro:

  def compileImpl[S <: Shape: Type, R: Type](cli: Expr[Cli[S, R]])(using Quotes): Expr[Parser[R]] =
    Compiler().build[S, R](cli)

  // -------------------------------------------------------------------------------------------
  // Macro-time model
  //
  // `Node` is the shape type decoded into plain data (no reflect types beyond this point), with
  // every payload already assigned its index in `Payloads.collect`'s preorder. `Scope` is one
  // command level after analysis, and `Build` is the recipe for reconstructing that level's
  // result — the same tree the interpreter walks, minus the closures.
  // -------------------------------------------------------------------------------------------

  private enum Node:
    case Flag(name: String, short: Option[Char])
    case Opt(name: String, short: Option[Char], readIdx: Int)
    case Arg(name: String, readIdx: Int)
    case Both(left: Node, right: Node)
    case OneOf(left: Node, right: Node)
    case Sub(name: String, scopeIdx: Int, helpEnabled: Boolean, inner: Node)
    case Mapped(inner: Node, fIdx: Int)
    case Default(inner: Node, valueIdx: Int)
    case Repeated(inner: Node)
    case Pure(valueIdx: Int)

  private enum Arity:
    case Required
    case Defaulted(valueIdx: Int)
    case Repeated

  private enum OptKind:
    case Flag
    case Value(readIdx: Int, maps: List[Int], arity: Arity)

  private final case class OptionSlot(slot: Int, name: String, short: Option[Char], kind: OptKind):
    def decode(kind: OptKind.Value): Decode =
      Decode(slot, name, kind.readIdx, kind.maps, kind.arity)

  private final case class PositionalSlot(
    slot: Int,
    name: String,
    readIdx: Int,
    maps: List[Int],
    arity: Arity
  ):
    def decode: Decode = Decode(slot, name, readIdx, maps, arity)

  /** Everything the generated code needs to turn one raw token into one slot's value. */
  private final case class Decode(
    slot: Int,
    name: String,
    readIdx: Int,
    maps: List[Int],
    arity: Arity
  )

  private final case class RawSub(
    name: String,
    scopeIdx: Int,
    helpEnabled: Boolean,
    inner: Node,
    wraps: List[Int]
  )
  private final case class SubEntry(name: String, wraps: List[Int], scope: Scope)

  /** Where a scope's `--help` text comes from, decided entirely at compile time. */
  private enum HelpMode:
    /** The root scope renders the captured `Cli` value; it can never opt out (A9.1). */
    case Root

    /** A subcommand with `H = true` reads `Subscopes.collect`'s table by constant index (A8.6). */
    case Enabled(scopeIdx: Int)

    /** A subcommand with `H = false`: the generated scanner has no help branch at all (A9.2). */
    case Disabled

  private final case class Scope(
    slotCount: Int,
    options: List[OptionSlot],
    positionals: List[PositionalSlot],
    subs: Option[List[SubEntry]],
    result: Build,
    help: HelpMode
  )

  private enum Build:
    case FlagResult(slot: Int)
    case ValueResult(slot: Int, name: String, arity: Arity, positional: Boolean)
    case Pair(left: Build, right: Build)
    case Mapped(inner: Build, fIdx: Int)
    case Const(valueIdx: Int)
    case SubResult(expected: List[String])

  private final class Compiler(using q: Quotes):
    import q.reflect.*

    private val flagSym = TypeRepr.of[Shape.Flag[?, ?]].typeSymbol
    private val optSym = TypeRepr.of[Shape.Opt[?, ?, ?]].typeSymbol
    private val argSym = TypeRepr.of[Shape.Arg[?, ?]].typeSymbol
    private val bothSym = TypeRepr.of[Shape.Both[?, ?]].typeSymbol
    private val oneOfSym = TypeRepr.of[Shape.OneOf[?, ?]].typeSymbol
    private val subSym = TypeRepr.of[Shape.Sub[?, ?, ?]].typeSymbol
    private val mappedSym = TypeRepr.of[Shape.Mapped[?]].typeSymbol
    private val defaultSym = TypeRepr.of[Shape.Default[?]].typeSymbol
    private val repeatedSym = TypeRepr.of[Shape.Repeated[?]].typeSymbol
    private val pureSym = TypeRepr.of[Shape.Pure].typeSymbol

    def build[S <: Shape: Type, R: Type](cli: Expr[Cli[S, R]]): Expr[Parser[R]] =
      var counter = 0
      val next = () =>
        val index = counter
        counter += 1
        index

      var scopeCounter = 0
      val nextScope = () =>
        val index = scopeCounter
        scopeCounter += 1
        index

      val root = analyze(decode(TypeRepr.of[S], next, nextScope), HelpMode.Root)
      '{
        val described = $cli
        val payloads = Payloads.collect(described)
        val subscopes = Subscopes.collect(described)
        new ParserRuntime.CompiledParser[R](
          described,
          (args: Seq[String]) =>
            ${ scopeExpr(root, 'args, 'payloads, HelpSources('described, 'subscopes)) }
        )
      }

    // -----------------------------------------------------------------------------------------
    // Decoding the shape type
    // -----------------------------------------------------------------------------------------

    /** Walk `S` in exactly [[Payloads.collect]]'s preorder — own payload first, then children left
      * to right — so a node's payload index here is the index it will occupy at runtime. The same
      * walk numbers the subcommand scopes for [[Subscopes.collect]], which contributes at `Sub`
      * nodes only; `nextScope` is therefore drawn before descending into the inner grammar.
      */
    private def decode(tpe: TypeRepr, next: () => Int, nextScope: () => Int): Node =
      tpe.dealias match
        case applied @ AppliedType(tycon, args) =>
          val sym = tycon.typeSymbol
          if sym == flagSym then Node.Flag(literalName(args(0)), literalShort(args(1)))
          else if sym == optSym then
            val readIdx = next()
            Node.Opt(literalName(args(0)), literalShort(args(1)), readIdx)
          else if sym == argSym then
            val readIdx = next()
            Node.Arg(literalName(args(0)), readIdx)
          else if sym == bothSym then
            val left = decode(args(0), next, nextScope)
            val right = decode(args(1), next, nextScope)
            Node.Both(left, right)
          else if sym == oneOfSym then
            val left = decode(args(0), next, nextScope)
            val right = decode(args(1), next, nextScope)
            Node.OneOf(left, right)
          else if sym == subSym then
            val scopeIdx = nextScope()
            val helpEnabled = literalBool(args(2))
            Node.Sub(literalName(args(0)), scopeIdx, helpEnabled, decode(args(1), next, nextScope))
          else if sym == mappedSym then
            val fIdx = next()
            Node.Mapped(decode(args(0), next, nextScope), fIdx)
          else if sym == defaultSym then
            val valueIdx = next()
            Node.Default(decode(args(0), next, nextScope), valueIdx)
          else if sym == repeatedSym then Node.Repeated(decode(args(0), next, nextScope))
          else notConcrete(applied)
        case other =>
          if other.typeSymbol == pureSym then Node.Pure(next())
          else notConcrete(other)

    private def literalName(tpe: TypeRepr): String =
      tpe.dealias match
        case ConstantType(StringConstant(value)) => value
        case other                               => notConcrete(other)

    /** A9.6: `Sub`'s help flag must be a literal, so `help = someBoolean` is a non-concrete shape
      * and takes the ordinary §7 abort rather than silently defaulting either way.
      */
    private def literalBool(tpe: TypeRepr): Boolean =
      tpe.dealias match
        case ConstantType(BooleanConstant(value)) => value
        case other                                => notConcrete(other)

    private def literalShort(tpe: TypeRepr): Option[Char] =
      tpe.dealias match
        case ConstantType(CharConstant(value)) => Some(value)
        case other if other =:= TypeRepr.of[Unit] => None
        case other                                => notConcrete(other)

    private def notConcrete(tpe: TypeRepr): Nothing =
      report.errorAndAbort(
        "optparse: the CLI shape must be statically known. Avoid type ascriptions that widen the " +
          "shape (e.g. `val c: Cli[?, T]`) and use literal option names. Offending type: " +
          tpe.show
      )

    private def abort(message: String): Nothing =
      report.errorAndAbort(s"optparse: $message")

    // -----------------------------------------------------------------------------------------
    // Scope analysis — where R1-R5 are enforced
    //
    // This mirrors `Interp.build` node for node, including the order in which violations are
    // reported: R1 (option names), R2 (subcommand-rooted branches, duplicate command names) and
    // R4 (what Default/Repeated may wrap) fire while walking the scope; R3 (one subcommand group,
    // no positionals beside it) and R5 (positional ordering) fire once the walk is complete;
    // sub-scopes are only analyzed after the enclosing scope is known to be valid.
    // -----------------------------------------------------------------------------------------

    private def analyze(root: Node, help: HelpMode): Scope =
      var slotCount = 0
      val options = ListBuffer.empty[OptionSlot]
      val positionals = ListBuffer.empty[PositionalSlot]
      val groups = ListBuffer.empty[List[RawSub]]

      def newSlot(): Int =
        val index = slotCount
        slotCount += 1
        index

      def registerOption(option: OptionSlot): Unit =
        // R6/A8.4: the generated scanner answers `--help` in every scope, so a user option cannot
        // claim the name. Short 'h' stays free, and Arg/Sub names are unaffected. The literal is
        // duplicated from `Help.reservedOptionName`, which macro-time code must not depend on.
        // A9.3: the reservation is universal — a scope that opted out of auto-help still may not
        // define its own `--help`, so that re-enabling it later cannot break a working grammar.
        if option.name == "help" then
          abort("option name 'help' is reserved for the automatic help option")
        if options.exists(_.name == option.name) then
          abort(s"duplicate long option name '--${option.name}' in scope")
        option.short.foreach { short =>
          if options.exists(_.short.contains(short)) then
            abort(s"duplicate short option name '-$short' in scope")
        }
        options += option

      def annotated(inner: Node, arity: Arity, annotation: String): Build =
        unwrapAnnotated(inner, annotation) match
          case (Node.Opt(name, short, readIdx), maps) =>
            val slot = newSlot()
            registerOption(OptionSlot(slot, name, short, OptKind.Value(readIdx, maps, arity)))
            Build.ValueResult(slot, name, arity, positional = false)
          case (Node.Arg(name, readIdx), maps) =>
            val slot = newSlot()
            positionals += PositionalSlot(slot, name, readIdx, maps, arity)
            Build.ValueResult(slot, name, arity, positional = true)
          case (other, _) =>
            abort(s"$annotation may wrap only an Opt or Arg (found ${kindOf(other)})")

      def walk(node: Node): Build =
        node match
          case Node.Flag(name, short) =>
            val slot = newSlot()
            registerOption(OptionSlot(slot, name, short, OptKind.Flag))
            Build.FlagResult(slot)

          case Node.Opt(name, short, readIdx) =>
            val slot = newSlot()
            registerOption(OptionSlot(slot, name, short, OptKind.Value(readIdx, Nil, Arity.Required)))
            Build.ValueResult(slot, name, Arity.Required, positional = false)

          case Node.Arg(name, readIdx) =>
            val slot = newSlot()
            positionals += PositionalSlot(slot, name, readIdx, Nil, Arity.Required)
            Build.ValueResult(slot, name, Arity.Required, positional = true)

          case Node.Both(left, right) =>
            val first = walk(left)
            val second = walk(right)
            Build.Pair(first, second)

          case Node.Mapped(inner, fIdx) =>
            Build.Mapped(walk(inner), fIdx)

          case Node.Default(inner, valueIdx) =>
            annotated(inner, Arity.Defaulted(valueIdx), "Default")

          case Node.Repeated(inner) =>
            annotated(inner, Arity.Repeated, "Repeated")

          case Node.Pure(valueIdx) =>
            Build.Const(valueIdx)

          case Node.Sub(_, _, _, _) | Node.OneOf(_, _) =>
            val raw = collectSubs(node, Nil)
            val seen = ListBuffer.empty[String]
            raw.foreach { entry =>
              if seen.contains(entry.name) then
                abort(s"duplicate subcommand name '${entry.name}' in group")
              seen += entry.name
            }
            groups += raw
            Build.SubResult(raw.map(_.name))

      val result = walk(root)

      if groups.size > 1 then abort("a scope may contain at most one subcommand group")

      if groups.nonEmpty && positionals.nonEmpty then
        abort(
          s"positional argument '${positionals.head.name}' may not be used in a scope with a subcommand group"
        )

      val repeated = positionals.filter(_.arity == Arity.Repeated).toList
      if repeated.size > 1 then
        abort(
          s"at most one repeated positional is allowed in a scope; '${repeated(1).name}' is also repeated"
        )
      repeated.headOption.foreach { slot =>
        if positionals.last.slot != slot.slot then
          abort(s"repeated positional '${slot.name}' must be last")
      }

      var firstDefaulted: Option[String] = None
      positionals.foreach { slot =>
        slot.arity match
          case Arity.Defaulted(_) =>
            if firstDefaulted.isEmpty then firstDefaulted = Some(slot.name)
          case Arity.Required =>
            firstDefaulted.foreach { previous =>
              abort(
                s"required positional '${slot.name}' may not follow defaulted positional '$previous'"
              )
            }
          case Arity.Repeated => ()
      }

      val entries = groups.headOption.map(_.map { raw =>
        val innerHelp =
          if raw.helpEnabled then HelpMode.Enabled(raw.scopeIdx) else HelpMode.Disabled
        SubEntry(raw.name, raw.wraps, analyze(raw.inner, innerHelp))
      })

      Scope(slotCount, options.toList, positionals.toList, entries, result, help)

    /** Peel the `Mapped` nodes below a `Default`/`Repeated`. The returned maps are outermost-first;
      * they are applied to each supplied occurrence, innermost first, exactly as the interpreter's
      * composed transform does.
      */
    private def unwrapAnnotated(node: Node, annotation: String): (Node, List[Int]) =
      node match
        case Node.Mapped(inner, fIdx) =>
          val (leaf, maps) = unwrapAnnotated(inner, annotation)
          (leaf, fIdx :: maps)
        case leaf @ Node.Opt(_, _, _) => (leaf, Nil)
        case leaf @ Node.Arg(_, _)    => (leaf, Nil)
        case Node.Flag(name, _)       => abort(s"--$name: $annotation may not wrap a Flag")
        case Node.Default(_, _)       => abort(s"$annotation may not be stacked with Default")
        case Node.Repeated(_)         => abort(s"$annotation may not be stacked with Repeated")
        case other => abort(s"$annotation may wrap only an Opt or Arg (found ${kindOf(other)})")

    /** Flatten one subcommand group. A map inside a branch wraps that branch's result only; a map
      * above the whole group is an ordinary `Build.Mapped` handled by `walk`.
      */
    private def collectSubs(node: Node, wraps: List[Int]): List[RawSub] =
      node match
        case Node.Sub(name, scopeIdx, helpEnabled, inner) =>
          List(RawSub(name, scopeIdx, helpEnabled, inner, wraps))
        case Node.OneOf(left, right)  => collectSubs(left, wraps) ++ collectSubs(right, wraps)
        case Node.Mapped(inner, fIdx) => collectSubs(inner, wraps :+ fIdx)
        case other => abort(s"OneOf branch must be subcommand-rooted (found ${kindOf(other)})")

    private def kindOf(node: Node): String =
      node match
        case Node.Flag(_, _)     => "Flag"
        case Node.Opt(_, _, _)   => "Opt"
        case Node.Arg(_, _)      => "Arg"
        case Node.Both(_, _)     => "Both"
        case Node.OneOf(_, _)    => "OneOf"
        case Node.Sub(_, _, _, _) => "Sub"
        case Node.Mapped(_, _)   => "Mapped"
        case Node.Default(_, _)  => "Default"
        case Node.Repeated(_)    => "Repeated"
        case Node.Pure(_)        => "Pure"

    // -----------------------------------------------------------------------------------------
    // Code generation
    //
    // One scope becomes one `Scan` plus a token loop whose branches are all decided statically:
    // option lookup is a chain of string comparisons on constant names, subcommand dispatch a
    // chain on constant command names, and every slot is a constant index into the scan's array.
    // Nested scopes are generated inline at their dispatch site.
    // -----------------------------------------------------------------------------------------

    private type Scan = ParserRuntime.Scan

    /** The two runtime handles a generated scope needs to answer `--help` (A8.6): the captured root
      * `Cli` for the root scope, and the `Subscopes.collect` table for every nested one. Rendering
      * stays inside the failure branch, so a parse that never sees `--help` never builds help text.
      */
    private final case class HelpSources(
      rootCli: Expr[Cli[?, ?]],
      subscopes: Expr[IArray[Cli[?, ?]]]
    ):
      /** `None` for a help-disabled scope, whose scanner gets no help branch at all (A9.2). The
        * two rendering sites exist only for help-enabled scopes, so both pass the scope's own `H`
        * (A9.4) as `includeHelpRow = true`.
        */
      def render(scope: Scope): Option[Expr[String]] =
        scope.help match
          case HelpMode.Root           => Some('{ Help.render($rootCli, true) })
          case HelpMode.Enabled(index) => Some('{ Help.render(${ subscopes }(${ Expr(index) }), true) })
          case HelpMode.Disabled       => None

    private def scopeExpr(
      scope: Scope,
      args: Expr[Seq[String]],
      payloads: Expr[IArray[Any]],
      help: HelpSources
    ): Expr[Either[ParseError, Any]] =
      '{
        val st = new ParserRuntime.Scan($args, ${ Expr(scope.slotCount) })
        while !st.exhausted && st.scanning && st.ok do
          val token = st.token
          if !st.terminated && token == "--" then st.terminate()
          else if st.terminated || ParserRuntime.isPositionalLike(token) then
            ${ positionalOrDispatch(scope, 'st, 'token, payloads, help) }
          else if token.startsWith("--") then
            ${ longOption(scope, 'st, 'token, payloads, help.render(scope)) }
          else if token.length == 2 then ${ shortOption(scope, 'st, 'token, payloads) }
          else st.fail(ParseError.UnknownOption(token))

        if st.failed then Left[ParseError, Any](st.failure)
        else ${ resultExpr(scope.result, 'st, payloads) }
      }

    private def positionalOrDispatch(
      scope: Scope,
      st: Expr[Scan],
      token: Expr[String],
      payloads: Expr[IArray[Any]],
      help: HelpSources
    ): Expr[Unit] =
      scope.subs match
        case Some(entries) => dispatchExpr(entries, entries.map(_.name), st, token, payloads, help)
        case None          => positionalExpr(scope.positionals, 0, st, token, payloads)

    private def dispatchExpr(
      remaining: List[SubEntry],
      expected: List[String],
      st: Expr[Scan],
      token: Expr[String],
      payloads: Expr[IArray[Any]],
      help: HelpSources
    ): Expr[Unit] =
      remaining match
        case Nil =>
          '{ $st.fail(ParseError.UnknownSubcommand($token, ${ Expr(expected) })) }
        case entry :: rest =>
          '{
            if $token == ${ Expr(entry.name) } then
              val remainder = $st.rest
              $st.dispatched(${
                wrapExpr(
                  entry.wraps,
                  scopeExpr(entry.scope, 'remainder, payloads, help),
                  payloads
                )
              })
            else ${ dispatchExpr(rest, expected, st, token, payloads, help) }
          }

    /** Branch-local maps applied to a subcommand's result, innermost first. */
    private def wrapExpr(
      wraps: List[Int],
      inner: Expr[Either[ParseError, Any]],
      payloads: Expr[IArray[Any]]
    ): Expr[Either[ParseError, Any]] =
      wraps.foldRight(inner) { (index, acc) =>
        '{ $acc.map(value => ParserRuntime.transform($payloads(${ Expr(index) }), value)) }
      }

    private def positionalExpr(
      remaining: List[PositionalSlot],
      at: Int,
      st: Expr[Scan],
      token: Expr[String],
      payloads: Expr[IArray[Any]]
    ): Expr[Unit] =
      remaining match
        case Nil =>
          '{ $st.fail(ParseError.UnexpectedArgument($token)) }
        case slot :: rest =>
          val consume: Expr[Unit] =
            if slot.arity == Arity.Repeated then '{ () } else '{ $st.advancePositional() }
          '{
            if $st.positionalIndex == ${ Expr(at) } then
              ${ storeRead(slot.decode, st, token, payloads) }
              if $st.ok then
                $consume
                $st.advance(1)
            else ${ positionalExpr(rest, at + 1, st, token, payloads) }
          }

    private def longOption(
      scope: Scope,
      st: Expr[Scan],
      token: Expr[String],
      payloads: Expr[IArray[Any]],
      help: Option[Expr[String]]
    ): Expr[Unit] =
      '{
        val equalsAt = $token.indexOf('=')
        val name = if equalsAt < 0 then $token.substring(2) else $token.substring(2, equalsAt)
        val bound = equalsAt >= 0
        val boundValue = if equalsAt < 0 then "" else $token.substring(equalsAt + 1)
        ${ helpOrChain(scope, 'name, 'bound, 'boundValue, st, token, payloads, help) }
      }

    /** The body of the long-option branch: the option chain, preceded by the help check when this
      * scope answers `--help`.
      */
    private def helpOrChain(
      scope: Scope,
      name: Expr[String],
      bound: Expr[Boolean],
      boundValue: Expr[String],
      st: Expr[Scan],
      token: Expr[String],
      payloads: Expr[IArray[Any]],
      help: Option[Expr[String]]
    ): Expr[Unit] =
      val chain = longChain(scope.options, name, bound, boundValue, st, token, payloads)
      help match
        // A8.1/A8.7: help is a scanner-level slot rather than a user option, so it is answered
        // before the option chain and it stops the scan — which is what makes it beat every
        // missing-check (A8.3). It still behaves like a flag for `=value` (A8.2).
        case Some(text) =>
          '{
            if $name == "help" then
              if $bound then
                $st.fail(
                  ParseError.InvalidValue("help", $boundValue, "flag does not take a value")
                )
              else $st.fail(ParseError.HelpRequested($text))
            else $chain
          }
        // A9.2: a help-disabled scope emits no help branch at all, so `--help` and `--help=x` reach
        // the option chain and fall out of it as ordinary unknown options, token as written.
        case None => chain

    private def longChain(
      remaining: List[OptionSlot],
      name: Expr[String],
      bound: Expr[Boolean],
      boundValue: Expr[String],
      st: Expr[Scan],
      token: Expr[String],
      payloads: Expr[IArray[Any]]
    ): Expr[Unit] =
      remaining match
        case Nil =>
          '{ $st.fail(ParseError.UnknownOption($token)) }
        case option :: rest =>
          '{
            if $name == ${ Expr(option.name) } then
              ${ longBody(option, bound, boundValue, st, payloads) }
            else ${ longChain(rest, name, bound, boundValue, st, token, payloads) }
          }

    private def longBody(
      option: OptionSlot,
      bound: Expr[Boolean],
      boundValue: Expr[String],
      st: Expr[Scan],
      payloads: Expr[IArray[Any]]
    ): Expr[Unit] =
      option.kind match
        case OptKind.Flag =>
          '{
            if $bound then
              $st.fail(
                ParseError.InvalidValue(
                  ${ Expr(option.name) },
                  $boundValue,
                  "flag does not take a value"
                )
              )
            else $st.flagSeen(${ Expr(option.slot) })
          }
        case kind: OptKind.Value =>
          '{
            if $bound then
              ${ addOption(option, kind, st, boundValue, payloads) }
              if $st.ok then $st.advance(1)
            else if !$st.hasNextToken then
              $st.fail(ParseError.MissingValue(${ Expr(option.name) }))
            else
              ${ addOption(option, kind, st, '{ $st.nextToken }, payloads) }
              if $st.ok then $st.advance(2)
          }

    private def shortOption(
      scope: Scope,
      st: Expr[Scan],
      token: Expr[String],
      payloads: Expr[IArray[Any]]
    ): Expr[Unit] =
      '{
        val short = $token.charAt(1)
        ${ shortChain(scope.options.filter(_.short.isDefined), 'short, st, token, payloads) }
      }

    private def shortChain(
      remaining: List[OptionSlot],
      short: Expr[Char],
      st: Expr[Scan],
      token: Expr[String],
      payloads: Expr[IArray[Any]]
    ): Expr[Unit] =
      remaining match
        case Nil =>
          '{ $st.fail(ParseError.UnknownOption($token)) }
        case option :: rest =>
          '{
            if $short == ${ Expr(option.short.get) } then
              ${ shortBody(option, st, payloads) }
            else ${ shortChain(rest, short, st, token, payloads) }
          }

    private def shortBody(
      option: OptionSlot,
      st: Expr[Scan],
      payloads: Expr[IArray[Any]]
    ): Expr[Unit] =
      option.kind match
        case OptKind.Flag =>
          '{ $st.flagSeen(${ Expr(option.slot) }) }
        case kind: OptKind.Value =>
          '{
            if !$st.hasNextToken then $st.fail(ParseError.MissingValue(${ Expr(option.name) }))
            else
              ${ addOption(option, kind, st, '{ $st.nextToken }, payloads) }
              if $st.ok then $st.advance(2)
          }

    private def addOption(
      option: OptionSlot,
      kind: OptKind.Value,
      st: Expr[Scan],
      raw: Expr[String],
      payloads: Expr[IArray[Any]]
    ): Expr[Unit] =
      val decode = option.decode(kind)
      kind.arity match
        case Arity.Repeated =>
          '{
            val rawValue = $raw
            ${ storeRead(decode, st, 'rawValue, payloads) }
          }
        case _ =>
          '{
            if $st.isSet(${ Expr(option.slot) }) then
              $st.fail(ParseError.DuplicateOption(${ Expr(option.name) }))
            else
              val rawValue = $raw
              ${ storeRead(decode, st, 'rawValue, payloads) }
          }

    /** Decode one raw token into a slot, or fail with the slot's bare name. */
    private def storeRead(
      decode: Decode,
      st: Expr[Scan],
      raw: Expr[String],
      payloads: Expr[IArray[Any]]
    ): Expr[Unit] =
      '{
        ParserRuntime.read($payloads(${ Expr(decode.readIdx) }), $raw) match
          case Right(decoded) =>
            ${
              val mapped = applyMaps(decode.maps, 'decoded, payloads)
              decode.arity match
                case Arity.Repeated => '{ $st.append(${ Expr(decode.slot) }, $mapped) }
                case _              => '{ $st.store(${ Expr(decode.slot) }, $mapped) }
            }
          case Left(message) =>
            $st.fail(ParseError.InvalidValue(${ Expr(decode.name) }, $raw, message))
      }

    /** Per-occurrence maps below a `Default`/`Repeated`, applied innermost first. */
    private def applyMaps(
      maps: List[Int],
      value: Expr[Any],
      payloads: Expr[IArray[Any]]
    ): Expr[Any] =
      maps.foldRight(value) { (index, acc) =>
        '{ ParserRuntime.transform($payloads(${ Expr(index) }), $acc) }
      }

    private def resultExpr(
      build: Build,
      st: Expr[Scan],
      payloads: Expr[IArray[Any]]
    ): Expr[Either[ParseError, Any]] =
      build match
        case Build.FlagResult(slot) =>
          '{ Right[ParseError, Any]($st.isSet(${ Expr(slot) })) }

        case Build.ValueResult(slot, name, arity, positional) =>
          arity match
            case Arity.Required =>
              val missing: Expr[ParseError] =
                if positional then '{ ParseError.MissingArgument(${ Expr(name) }) }
                else '{ ParseError.MissingOption(${ Expr(name) }) }
              '{
                if $st.isSet(${ Expr(slot) }) then Right[ParseError, Any]($st.valueOf(${ Expr(slot) }))
                else Left[ParseError, Any]($missing)
              }
            case Arity.Defaulted(valueIdx) =>
              '{
                if $st.isSet(${ Expr(slot) }) then Right[ParseError, Any]($st.valueOf(${ Expr(slot) }))
                else Right[ParseError, Any]($payloads(${ Expr(valueIdx) }))
              }
            case Arity.Repeated =>
              '{ Right[ParseError, Any]($st.collected(${ Expr(slot) })) }

        case Build.Pair(left, right) =>
          '{
            ${ resultExpr(left, st, payloads) }.flatMap { first =>
              ${ resultExpr(right, st, payloads) }.map(second => (first, second))
            }
          }

        case Build.Mapped(inner, fIdx) =>
          '{
            ${ resultExpr(inner, st, payloads) }.map(value =>
              ParserRuntime.transform($payloads(${ Expr(fIdx) }), value)
            )
          }

        case Build.Const(valueIdx) =>
          '{ Right[ParseError, Any]($payloads(${ Expr(valueIdx) })) }

        case Build.SubResult(expected) =>
          '{
            if $st.hasSub then $st.sub
            else Left[ParseError, Any](ParseError.MissingSubcommand(${ Expr(expected) }))
          }
