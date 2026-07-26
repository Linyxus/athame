package optparse

import scala.collection.mutable.ArrayBuffer

/** Deterministic collection of every subcommand scope in a [[Cli]] tree (A8.6).
  *
  * The walk is the same preorder as [[Payloads.collect]] — a node contributes before recursing, and
  * children are visited left to right — but the only contributing node kind is `Sub`, which appends
  * its inner grammar:
  *
  * {{{
  * Sub -> [inner]        everything else -> []
  * }}}
  *
  * This is the contract that lets `--help` inside a nested scope render that scope's grammar without
  * the generated parser ever traversing the tree: [[Compile.compile]] walks the phantom shape in
  * exactly this order, assigns each `Sub` its index statically, and the generated help trigger reads
  * the scope back by constant index. The traversal must stay identical to `Payloads`' — same node
  * order, different payload — so keep the two walks side by side and change them together.
  *
  * A9.5: every `Sub` inner is collected, including scopes that have opted out of `--help`. Making
  * the walk help-dependent would make the indices depend on the grammar's help flags. An opted-out
  * scope's entry is read too, just never for a `--help` token: its subcommand errors carry its help
  * as error context (A10.2), rendered without the row advertising the option it refuses.
  */
object Subscopes:

  def collect(cli: Cli[?, ?]): IArray[Cli[?, ?]] =
    val out = ArrayBuffer.empty[Cli[?, ?]]
    walk(cli, out)
    IArray.unsafeFromArray(out.toArray)

  private def walk(node: Cli[?, ?], out: ArrayBuffer[Cli[?, ?]]): Unit =
    node match
      case Cli.Flag(_, _, _) =>
        ()
      case Cli.Opt(_, _, _, _) =>
        ()
      case Cli.Arg(_, _, _) =>
        ()
      case Cli.Both(left, right) =>
        walk(left, out)
        walk(right, out)
      case Cli.OneOf(left, right) =>
        walk(left, out)
        walk(right, out)
      case Cli.Sub(_, _, inner, _) =>
        out += inner
        walk(inner, out)
      case Cli.Mapped(inner, _) =>
        walk(inner, out)
      case Cli.Default(inner, _) =>
        walk(inner, out)
      case Cli.Repeated(inner) =>
        walk(inner, out)
      case Cli.Pure(_) =>
        ()
