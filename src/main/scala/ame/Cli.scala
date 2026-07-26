package ame

import optparse.{flag, opt, pure, sub}

/** The command tree, as data.
  *
  * This file only describes the grammar; it never compiles it. `compile` is a macro that reads the
  * inferred shape type of what it is handed, so the tree lives here as a plain `val` and Main.scala
  * is the one place in the main sources that turns it into a parser.
  *
  * A parse produces `Command | ServeConfig`, which is what `|` types an alternation as. The union
  * is not a wart: `serve` is not a one-shot command and must never reach [[Runner.execute]], whose
  * whole contract — produce an [[Outcome]], print it, exit — is precisely what a server is not.
  * Making it a [[Command]] case would put a server behind a function that promises to return.
  *
  * Two rules govern edits here, both learned the hard way and both invisible until violated:
  *
  *   - No type ascription anywhere in [[cli]] or anything it is built from. The macro reads the
  *     inferred `Shape` type, and even a widening ascription like `val cli: optparse.Cli[?, Command]`
  *     erases it, at which point `compile` fails with "shape must be statically known".
  *   - Build with the combinators that are members of `optparse.Cli` (`|`, `~`, `map`). They are
  *     members rather than extension methods on purpose; see the note in optparse/Cli.scala.
  *
  * `version` opts out of the automatic `--help` and keeps doing so: `ame version --help` has always
  * been an unknown-option error rather than a help screen, and that is a promise about a released
  * command line, not an oversight.
  */
object Cli:

  val cli =
    sub("version", "Print the version number", pure(Command.Version), help = false) |
      sub(
        "sync",
        "Manage sync generations",
        sub(
          "begin",
          "Begin a new generation (records the pre-sync snapshot)",
          pure(Command.SyncBegin)
        ) | sub(
          "commit",
          "Commit the open generation (records the post-sync snapshot)",
          pure(Command.SyncCommit)
        ) | sub(
          "abort",
          "Abort the open generation",
          pure(Command.SyncAbort)
        ) | sub(
          "list",
          "List generations, oldest first",
          pure(Command.SyncList)
        ) | sub(
          "log",
          "Show generation details, newest first",
          pure(Command.SyncLog)
        ) | sub(
          "diff",
          "Show changes since the last completed generation",
          pure(Command.SyncDiff)
        )
      ) | sub(
        "serve",
        "Serve athame's tools over MCP (stdio by default)",
        (flag("http", "Serve over Streamable HTTP instead of stdio")
          ~ opt[String]("host", "Bind address for --http (default 127.0.0.1)").optional
          ~ opt[Int]("port", "Port for --http (default 3000)").optional)
          .map { case ((http, host), port) => ServeConfig(http, host, port) }
      )
