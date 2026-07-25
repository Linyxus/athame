package optparse

import scala.collection.mutable.ArrayBuffer

/** Deterministic collection of the runtime payloads carried by a [[Cli]] tree.
  *
  * The walk is a preorder over the tree: a node appends its own payloads *before* recursing into its
  * children, and children are visited left to right. Per node kind:
  *
  * {{{
  * Opt      -> [read]        Mapped   -> [f]        Pure -> [value]
  * Arg      -> [read]        Default  -> [default]  everything else -> []
  * }}}
  *
  * This order is the contract between the runtime and the macro: [[Compile.compile]] walks the
  * phantom shape type in exactly the same order and bakes the resulting indices into the generated
  * parser, which then reads its payloads back by constant index. Changing the walk here without
  * changing the macro (or vice versa) yields parsers that still compile but use the wrong payloads,
  * so keep this deliberately dumb and keep the two walks side by side.
  */
object Payloads:

  def collect(cli: Cli[?, ?]): IArray[Any] =
    val out = ArrayBuffer.empty[Any]
    walk(cli, out)
    IArray.unsafeFromArray(out.toArray)

  private def walk(node: Cli[?, ?], out: ArrayBuffer[Any]): Unit =
    node match
      case Cli.Flag(_, _, _) =>
        ()
      case Cli.Opt(_, _, _, read) =>
        out += read
      case Cli.Arg(_, _, read) =>
        out += read
      case Cli.Both(left, right) =>
        walk(left, out)
        walk(right, out)
      case Cli.OneOf(left, right) =>
        walk(left, out)
        walk(right, out)
      case Cli.Sub(_, _, inner) =>
        walk(inner, out)
      case Cli.Mapped(inner, f) =>
        out += f
        walk(inner, out)
      case Cli.Default(inner, default) =>
        out += default
        walk(inner, out)
      case Cli.Repeated(inner) =>
        walk(inner, out)
      case Cli.Pure(value) =>
        out += value
