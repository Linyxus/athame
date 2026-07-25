# athame

A Scala.js CLI application built around **optparse**: a typed DSL for command-line
parsing where the grammar lives in the type system and a macro elaborates it into a
specialized parser at compile time.

```scala
import optparse.*

case class Build(target: String, jobs: Int, verbose: Boolean)

val cli =
  sub(
    "build",
    "Compile a target",
    (arg[String]("target", "Target to build") ~
      opt[Int]("jobs", 'j', "Parallel jobs").withDefault(1) ~
      flag("verbose", 'v', "Chatty output"))
      .map { case ((target, jobs), verbose) => Build(target, jobs, verbose) }
  )

val parser = compile(cli)          // macro: validates the grammar, generates the parser
parser.parse(List("build", "core", "-j", "4"))
// Right(Build("core", 4, false))
```

The shape of the CLI is carried in a phantom type index, so plain `val` composition
preserves it. `compile` reads that type — never the term tree — which lets it reject
invalid grammars at compile time (duplicate option names, ambiguous subcommand
alternatives, misplaced positionals, misused `withDefault`/`repeated`, widened shapes)
and emit a token scanner with no runtime interpretation. A reference interpreter
(`Interp`) doubles as the executable specification; differential tests (including a
10k-vector fuzz) keep the two in exact agreement, error values included.

## Building

```
sbt test           # full suite (interpreter, macro, compile-error tests)
sbt packageBinary  # Node SEA binary at dist/ame
```

`packageBinary` bundles the Scala.js output with esbuild and injects it into a copy of
the local `node` executable, so `dist/ame` runs standalone.

## License

[MIT](LICENSE)
