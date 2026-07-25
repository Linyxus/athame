package optparse

import scala.collection.mutable.ListBuffer

/** Readable, deliberately simple help text for the root command scope. */
object Help:
  private final case class OptionDoc(name: String, short: Option[Char], desc: String)
  private final case class ArgumentDoc(name: String, desc: String, form: ArgumentForm)
  private final case class CommandDoc(name: String, desc: String)

  private enum ArgumentForm:
    case Required
    case Defaulted
    case Repeated

  private final class RootDoc:
    val options = ListBuffer.empty[OptionDoc]
    val arguments = ListBuffer.empty[ArgumentDoc]
    val commands = ListBuffer.empty[CommandDoc]

  def render(cli: Cli[?, ?]): String =
    val doc = new RootDoc
    collect(cli, doc)

    val usageTail =
      if doc.commands.nonEmpty then List("<command>")
      else doc.arguments.toList.map(argumentUsage)
    val usage = ("[options]" :: usageTail).mkString(" ")

    val out = new StringBuilder(s"Usage: $usage")
    appendSection(
      out,
      "Options",
      doc.options.toList.map { option =>
        val short = option.short.fold("")(char => s", -$char")
        (s"--${option.name}$short", option.desc)
      }
    )
    appendSection(
      out,
      "Arguments",
      doc.arguments.toList.map(argument => (argument.name, argument.desc))
    )
    appendSection(
      out,
      "Commands",
      doc.commands.toList.map(command => (command.name, command.desc))
    )
    out.append('\n').result()

  /** Walk only the root scope. A Sub's inner grammar is intentionally not traversed. */
  private def collect(node: Cli[?, ?], doc: RootDoc): Unit =
    node match
      case Cli.Flag(name, short, desc) =>
        doc.options += OptionDoc(name, short, desc)
      case Cli.Opt(name, short, desc, _) =>
        doc.options += OptionDoc(name, short, desc)
      case Cli.Arg(name, desc, _) =>
        doc.arguments += ArgumentDoc(name, desc, ArgumentForm.Required)
      case Cli.Both(left, right) =>
        collect(left, doc)
        collect(right, doc)
      case Cli.Mapped(inner, _) =>
        collect(inner, doc)
      case Cli.Default(inner, _) =>
        collectAnnotated(inner, ArgumentForm.Defaulted, doc)
      case Cli.Repeated(inner) =>
        collectAnnotated(inner, ArgumentForm.Repeated, doc)
      case Cli.Pure(_) =>
        ()
      case Cli.Sub(_, _, _) =>
        collectCommands(node, doc)
      case Cli.OneOf(_, _) =>
        collectCommands(node, doc)

  /** Default/Repeated annotations can legally have any number of Mapped nodes below them. For an
    * invalid tree, fall back to the ordinary traversal so help rendering remains best-effort.
    */
  private def collectAnnotated(
    node: Cli[?, ?],
    form: ArgumentForm,
    doc: RootDoc
  ): Unit =
    node match
      case Cli.Mapped(inner, _) =>
        collectAnnotated(inner, form, doc)
      case Cli.Opt(name, short, desc, _) =>
        doc.options += OptionDoc(name, short, desc)
      case Cli.Arg(name, desc, _) =>
        doc.arguments += ArgumentDoc(name, desc, form)
      case other =>
        collect(other, doc)

  private def collectCommands(node: Cli[?, ?], doc: RootDoc): Unit =
    node match
      case Cli.Sub(name, desc, _) =>
        doc.commands += CommandDoc(name, desc)
      case Cli.OneOf(left, right) =>
        collectCommands(left, doc)
        collectCommands(right, doc)
      case Cli.Mapped(inner, _) =>
        collectCommands(inner, doc)
      case _ =>
        ()

  private def argumentUsage(argument: ArgumentDoc): String =
    argument.form match
      case ArgumentForm.Required  => s"<${argument.name}>"
      case ArgumentForm.Defaulted => s"[<${argument.name}>]"
      case ArgumentForm.Repeated  => s"<${argument.name}>..."

  private def appendSection(
    out: StringBuilder,
    title: String,
    rows: List[(String, String)]
  ): Unit =
    if rows.nonEmpty then
      val width = rows.map(_._1.length).max
      out.append("\n\n").append(title).append(":\n")
      rows.foreach { (label, description) =>
        out
          .append("  ")
          .append(label)
          .append(" " * (width - label.length + 2))
          .append(description)
          .append('\n')
      }
