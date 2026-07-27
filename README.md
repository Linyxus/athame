# athame

Athame is a prototype CLI tool for working with Scala's [magic spec strings](https://nightly.scala-lang.org/docs/reference/experimental/spec-strings.html).

## Tutorial

First, clone the template repository:

``` sh
git clone https://github.com/Linyxus/magic-testrepo1.git
```

Explore it a bit. It is a small Scala.js project whose functions carry spec strings.

Then start Claude Code in the cloned repository and install the plugin:

```
/plugin marketplace add Linyxus/athame
/plugin install magic@athame
```

Run `/reload-plugins` (or start a fresh session) so the command and the MCP server are
loaded. Then, when you are ready:

```
/magic:sync
```

This is your first sync. It surveys every spec string in the repository, implements the
`???` bodies, asks you about the contradiction rather than guessing, checks that the
project still compiles, and records the whole pass as generation 1.

Then enjoy! Tweak the spec strings, or the implementations, or both, and run
`/magic:sync` again: whatever drifted since the last generation gets reconciled, and you watch the **magic** happen.

## Building

```
sbt test           # full suite (interpreter, macro, compile-error tests)
sbt packageBinary  # Node SEA binary at dist/ame
```

`packageBinary` bundles the Scala.js output with esbuild and injects it into a copy of
the local `node` executable, so `dist/ame` runs standalone.

## License

[MIT](LICENSE)
