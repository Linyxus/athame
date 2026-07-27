---
description: Reconcile Scala spec strings with the code under them and record the pass as an athame sync generation.
argument-hint: [files or functions to focus on]
---

# Sync specs and code

Bring every spec string in this repository into agreement with the code it specifies, and
record the pass as a numbered athame generation.

The focus argument for this run, empty if the user gave none, is between the markers:

<focus>$ARGUMENTS</focus>

## What a spec string is

Spec strings are an experimental Scala 3 feature, enabled per file by
`import language.experimental.magic` — *magic* being **m**odular **ag**ent **i**nterface
**c**ode. A spec string is a dedented triple-quote literal whose opening quotes are followed
immediately by the word `spec`. It comes after a definition's signature and before the body it
describes:

```scala
def parseDate(str: String): Option[Date] =
  '''spec
  Parse date string in the format "day/month/year" into a `Date` structure.
  '''
  ???
```

The quotes are three apostrophes, not backticks. What you need to know about them:

- **The spec is the contract and the code below it is the implementation.** A spec string
  governs the code that follows it, up to the end of the enclosing block or the next spec
  string, whichever comes first. This is the inverse of a doc comment: a comment describes code
  that already exists, a spec string states the intent the code is supposed to satisfy.
- **The block is markdown, and backticked content in it is soft-interpolated.** The compiler
  parses and type-checks whatever is in backticks against the enclosing scope, so `a` in a spec
  is the parameter `a` itself, not a quotation of its name: go-to-definition works on it and a
  rename of the parameter rewrites the spec too. Put parameter and identifier names in
  backticks for that reason. Backticked content that does not type-check still compiles; it
  only produces a warning.
- **Spec strings erase.** They expand to `()` and have no runtime meaning, so nothing about
  them can be observed by a test and nothing about them costs anything at run time.
- **A `???` body under a spec means "implement me".** The feature reuses Scala's `???` for
  exactly this, with the agent rather than the human doing the writing. Every `???` under a
  spec is sync work.
- **Code with no spec string is a legal state.** It is unspecified, not out of sync. Never
  invent a spec for an unspecified definition unless the user asks for one.
- **The import is an experimental language import**, so the definitions in the file are
  `@experimental` and that propagates to their callers. Projects deal with this by adding
  `-experimental` to `scalacOptions` or by writing the same import in the calling files. Sync
  work never needs to change any of it — but a compile error about experimental definitions has
  this cause, and its fix is toolchain configuration, never deleting a spec or an import.
- **Find them** by searching the source tree for `'''spec`.

## What athame records

athame records each sync as a numbered **generation**: a pair of snapshots bracketing the work.

- `sync_begin` opens a generation and snapshots the working tree as the sync found it.
  `sync_commit` closes it, snapshots the tree as the sync left it, and reports whether anything
  changed. `sync_abort` discards the open generation and its snapshot, as if it had never begun.
- `sync_amend` replaces the last completed generation's post-sync snapshot with the working tree
  as it stands now, as if that sync had ended here. It is the one recording tool that targets a
  **completed** generation rather than an open one, and it is for one narrow situation only: see
  *Amending a previous sync* below.
- At most one generation is open at a time. `sync_begin` fails while one is open, and
  `sync_commit` and `sync_abort` fail when none is. `sync_amend` fails while one is open as well,
  and when no generation has ever been completed.
- None of the four touches a file. Snapshots are commits under `refs/ame/` covering every
  tracked file plus every untracked file `.gitignore` does not exclude; branches, the index,
  HEAD and the working tree are all left alone. In particular **`sync_abort` does not revert
  anything** — it drops the record and leaves the edits where they are.
- The seven tools are the `sync_*` tools on the `ame` MCP server: `sync_begin`, `sync_commit`,
  `sync_amend` and `sync_abort` record, while `sync_list`, `sync_log` and `sync_diff` are
  read-only. None of them takes arguments, and none takes a path: the server fixed its repository
  at startup — the one this session is running in — and acts only there.
- `sync_diff` shows what has changed since the **last completed** generation's post-sync
  snapshot. That drift is precisely what a sync has to reconcile. An open generation is never
  the baseline, because it has no recorded after-state; run mid-sync, the diff therefore shows
  the human's edits and your own together, both measured from the last state at which spec and
  code were reconciled.

## Two rules that hold throughout

**Never run a git mutation.** No `add`, `commit`, `stash`, `checkout`, `restore`, `reset`,
`clean`, or branch operation, and no reverting of the user's edits by hand either. The
generation is the entire record this command keeps; whether the user's work becomes a commit is
the user's decision, taken later. Reading git — `status`, `diff`, `log`, `show` — is fine and
often useful.

**Never settle a contradiction on the user's behalf.** Reconciling a pair where one side is
merely incomplete is your job. Choosing between two incompatible human intentions is not: when
the spec claims one thing and the code does another and nothing available says which is
correct, stop for that definition and ask. Do not overwrite a human's edit silently, in either
direction.

## The procedure

### 1. Orient

Call `sync_list` first. It is read-only and it tells you which of three situations you are in.

- **`no generations`** — this repository has never been synced. Go to *first sync*.
- **A line ending in `open`** (`#3  open`) — a previous sync began and never finished. Work out
  what it was doing: `sync_log` for when it began and what it snapshotted, the working tree and
  `git status`/`git diff` for what is currently uncommitted. If the unfinished reconciliation is
  evident, finish it under the generation that is already open and `sync_commit` — do not open a
  second one, and note that `sync_begin` would refuse anyway. If it is not evident, present what
  you found and ask the user whether to continue that generation or abort it. **Never call
  `sync_abort` on your own initiative.**
- **Otherwise** — every generation is committed, each line reading either
  `#1  committed  changed` or `#2  committed  no-op`. Go to *incremental sync*.

### 2. Incremental sync

Call `sync_diff` **before** beginning anything: it is read-only, it needs no open generation,
and its baseline is exactly the state the last sync left. Read the patch definition by
definition, and note which side of each pair moved — the spec text, the code, or both.

Then search the tree for spec strings whose body is still `???`. A spec that was already
unimplemented when the last generation was committed is unchanged since, so it appears in no
diff — the survey is not optional.

If `sync_diff` answers `no changes since generation N` and no spec has a `???` body, report that
the repository is in sync, touch nothing, and **do not call `sync_begin`** — an empty generation
records nothing worth recording. If instead it fails with
`error: no completed generation to diff against`, there is no baseline at all; treat the run as
a first sync.

Otherwise: `sync_begin`, reconcile each implicated definition per the doctrine below, verify,
`sync_commit`.

### 3. First sync

There is no baseline and nothing to diff, so survey instead: find every `'''spec` in the source
tree and read each one together with the code beneath it. The work list is:

- **Every spec with a `???` body** — implement it.
- **Every spec with a written body** — read the code against the spec and judge whether it does
  what the spec says. A first sync is the only opportunity to catch a pair that was never
  reconciled at all.

With no history to arbitrate, a disagreement found this way cannot be classified as spec drift
or code drift — nobody can say which side moved. So:

- If one side is merely **incomplete** — the spec describes behavior the code does not cover
  yet, or the code does something the spec simply does not mention — and the two do not conflict,
  reconcile in the direction that loses no information.
- If the two make **contradictory** claims, edit neither side. A spec whose last sentence says
  an empty interval yields `lo` while the code returns `hi` is contradictory: either the
  sentence is wrong or the code is, and only the author knows which. Collect these, present each
  as *the spec says X, the code does Y*, and ask which side is intent **before** touching that
  definition.

If there is any work to do, `sync_begin` first, do it, verify, `sync_commit`. If the repository
contains no spec strings at all, say so and stop: there is nothing here for this command to act
on.

### 4. If the work cannot be finished

**Never commit a generation whose post state does not compile.** A committed generation is a
claim about where the sync landed, and a broken tree is not somewhere to land. If you cannot
finish, either fix it or explain precisely what is blocking. When abandoning the attempt, call
`sync_abort` and tell the user plainly that abort discards only the record: the partial edits
are still in their working tree, and reverting them is theirs to do, not yours.

## Amending a previous sync

Use this **only** when the user says a completed sync got something wrong and asks you to change
it. An amend is not a new sync and it is never a first resort: a repository that has drifted since
the last generation wants an incremental sync, and a generation that is still open wants
`sync_commit`. What amend is for is the narrower case where the sync is over, the user has read
what it did, and what it did was not right. The corrective edits then belong to that generation
rather than to a new one, and `sync_amend` is what says so — it moves the last completed
generation's post-sync snapshot to the working tree as it stands now, and discards the snapshot it
replaces. There is no history of a generation's earlier endings and no way back to one.

The work itself is a sync's, minus the bracketing:

- **Make the corrective edits the user asked for.** The reconciliation doctrine below governs them
  exactly as it governs a sync's, and a focus argument still confines them.
- **Verify before recording anything**, by the project's own build command, exactly as before a
  `sync_commit`. **Never amend a non-compiling tree** — a generation is a claim about where the
  sync landed, and that is as true of an amended one as of a fresh one.
- **Then call `sync_amend`**, so the generation records where the sync actually ended rather than
  where it first stopped. It answers `amended generation N (changed)`, or
  `amended generation N (no changes)` when the tree now matches the state that generation began
  from — which is what an amend that undid the whole sync looks like.

It refuses in two situations, and neither is to be worked around. While a generation is open it
answers `error: generation N is already open (commit or abort it first)`: the open generation is
the one to finish, and the one below it is not what the user meant. When nothing has ever been
completed it answers `error: no completed generation to amend`, which means this repository has no
sync to correct — run one instead.

Afterwards, `sync_diff` measures from the amended state: the snapshot just recorded is the baseline
the next run reconciles against, exactly as any other post-sync snapshot would be.

**Never amend on your own initiative.** An explicit request from the user is the only trigger for
it. A generation whose result you have doubts about is something to raise with them, not something
to quietly re-record. Both rules above hold here undiminished: no git mutation of any kind, and no
contradiction settled on the user's behalf.

## Reconciliation doctrine

For each definition the diff or the survey implicates:

| What moved | What to do |
| --- | --- |
| The spec, not the code | The spec is the new intent. Rewrite the body to satisfy it. |
| The code, not the spec | The code is the new intent. Rewrite the spec prose to describe the new behavior. |
| Both, compatibly | Reconcile them into a consistent pair — for instance the code was implemented while the spec was reworded, or both moved the same direction. |
| Both, contradictorily | Stop for that definition and ask the user which side is intent. |
| A new spec with `???` | Implement it. |
| Neither | Leave it alone. |

How to make the edits:

- **Minimal diffs.** Touch only the definitions the drift or the work list implicates. Do not
  reformat, rename, restructure, or improve anything else in the files you open.
- **Helper definitions are allowed** when an implementation genuinely needs one; the feature
  explicitly sanctions system-generated code beyond the body the spec sits above. Place the
  helper adjacent to the definition that uses it, keep its visibility as narrow as the language
  allows, and do not give it a spec string of its own unless the user asks.
- **Never weaken a spec to make broken code true.** If the code is wrong, fix the code. If you
  genuinely believe the spec is the side that should change, say so and ask — do not quietly
  edit the sentence that the code fails.
- **Preserve each spec's prose style**: complete sentences, markdown, identifiers in backticks,
  the dedented left margin intact and the closing `'''` where it was. A reconciled spec should
  read as though the person who wrote the original wrote it.
- **Respect the focus argument.** If the markers at the top of this file name files or
  functions, every edit stays inside them; anything else is left untouched even where it has
  drifted, and that drift is reported as skipped rather than fixed. The generation is recorded
  either way.

## Verification

Before `sync_commit`, the tree must build.

- Find the project's build command, in this order: the project's own instructions (`CLAUDE.md`
  and anything it points at), then the README, then the toolchain default — for an sbt project,
  `sbt compile`, and `sbt test` as well when the project has a test suite.
- Run it, and iterate on your edits until it is green.
- A failure that comes from the toolchain rather than from your edits — a compiler nightly that
  is not installed, a missing `-experimental` flag, an unresolved dependency — is a blocker to
  report to the user, not something to work around by deleting a spec string, dropping the
  `magic` import, or weakening code until it compiles.

## Reporting

After `sync_commit`, report what happened one definition at a time, naming for each which of
these it was: implemented, code updated, spec updated, asked and resolved, or skipped as out of
scope. Then close with the generation number that was recorded and whether the commit reported
changes.
