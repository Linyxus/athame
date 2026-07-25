# athame

## Building

```
sbt test           # full suite (interpreter, macro, compile-error tests)
sbt packageBinary  # Node SEA binary at dist/ame
```

`packageBinary` bundles the Scala.js output with esbuild and injects it into a copy of
the local `node` executable, so `dist/ame` runs standalone.

## License

[MIT](LICENSE)
