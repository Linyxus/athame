# athame

## Building

```
sbt test           # full suite (interpreter, macro, compile-error tests)
sbt packageBinary  # Node SEA binary at dist/ame
```

`packageBinary` bundles the Scala.js output with esbuild and injects it into a copy of
the local `node` executable, so `dist/ame` runs standalone.

## The magic plugin

This repository is also a Claude Code plugin marketplace, offering one plugin:

```
/plugin marketplace add Linyxus/athame
/plugin install magic@athame
```

`magic` ships athame as an MCP server — `plugin/bin/ame.cjs`, run with `node` — and one
command, `/sync`. That command surveys the `'''spec` strings in whichever repository
Claude Code is running in and reconciles each spec with the code below it: implementing
`???` bodies, rewriting code where the spec moved, rewriting prose where the code moved,
and asking the user rather than guessing when the two contradict each other. It then
verifies that the project still builds and records the whole pass as a numbered athame
generation. It never runs a git mutation.

`plugin/bin/ame.cjs` is a committed build artifact; refresh it with `sbt packagePlugin`
whenever athame's behavior changes. That task and `packageBinary` share one esbuild
bundling step, so the plugin ships exactly what `dist/ame` runs.

## License

[MIT](LICENSE)
