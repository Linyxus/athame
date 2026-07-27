# magic

A Claude Code plugin for [athame](https://github.com/Linyxus/athame): it keeps a Scala
project's spec strings and the code under them saying the same thing.

## What it ships

- **The `ame` MCP server.** `bin/ame.cjs` is athame bundled into a single dependency-free
  CommonJS file, run with whatever `node` is on your PATH. It offers seven tools —
  `sync_begin`, `sync_commit`, `sync_amend`, `sync_abort`, `sync_list`, `sync_log`, `sync_diff` —
  which record and inspect athame's numbered sync generations.
- **`/sync`.** One command that surveys the `'''spec` strings in the repository,
  reconciles each spec with its implementation, verifies that the project still builds, and
  records the pass as a generation.

## Install

```
/plugin marketplace add Linyxus/athame
/plugin install magic@athame
```

Node has to be on your PATH. Nothing else is required.

## The server acts on the repository you are in

`ame serve` takes no repository argument, and none of its tools takes a path. The server fixes
its repository at startup — the directory Claude Code launched it in — and every call acts
there. To sync a different project, run Claude Code in that project.

Its snapshots are commits under `refs/ame/`, so they never disturb your branches, your index, or
your working tree, and `/sync` itself never runs a git mutation of any kind.

## Refreshing the bundle

`bin/ame.cjs` is a committed build artifact. From an athame checkout, `sbt packagePlugin`
rebuilds it.
