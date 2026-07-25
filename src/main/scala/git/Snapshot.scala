package git

import scala.scalajs.js

/** The commit produced by [[Snapshot.create]].
  *
  * `parent` is empty exactly when the repository had an unborn HEAD. `sameAsHeadTree` reports that
  * the snapshot recorded the same tree as `HEAD^{tree}`, i.e. the working tree held nothing that
  * HEAD does not already contain — the "there was nothing to sync" signal. It is `false` whenever
  * HEAD is unborn, since there is no tree to compare against.
  */
final case class SnapshotCreated(
  id: String,
  ref: String,
  commit: String,
  tree: String,
  parent: Option[String],
  sameAsHeadTree: Boolean
)

/** One snapshot as reported by [[Snapshot.list]]: the id with the commit its ref points at. */
final case class SnapshotEntry(id: String, commit: String)

/** Non-disruptive snapshots of a git working directory.
  *
  * A snapshot is an ordinary commit recording the current state of the whole repository — every
  * tracked file plus every untracked file that `.gitignore` does not exclude, exactly what `git add
  * -A` would stage — parented on the current HEAD and reachable only through `refs/ame/snapshots/`.
  * That namespace is deliberately outside `refs/heads/`, so snapshots never show up in `git branch`
  * and never compete with the user's own branches.
  *
  * Creating one leaves the repository untouched: HEAD, the branches, the user's index and the
  * working tree are all exactly as they were, and staged-but-uncommitted work, unstaged edits,
  * untracked files and an in-progress merge or rebase all survive bit-for-bit. The mechanism is a
  * throwaway index file in the OS temp directory — `read-tree` HEAD into it, `add -A` against it,
  * `write-tree` out of it — so the real `.git/index` is never opened for writing. The commit itself
  * is made with `commit-tree` rather than porcelain `commit`: that bypasses hooks by design, because
  * a snapshot is bookkeeping and must not be vetoable by a `pre-commit` script.
  *
  * Author and committer are fixed to `ame <ame@snapshot>` and passed through the environment, so
  * snapshots work in repositories where the user never configured a git identity.
  *
  * Submodules are out of scope for v1: a submodule is recorded as its gitlink, exactly as git stores
  * it, and its own working tree is not recursed into or snapshotted.
  */
object Snapshot:

  /** Hidden namespace holding every snapshot ref. */
  val RefPrefix: String = "refs/ame/snapshots/"

  private val Identity = "ame"
  private val IdentityEmail = "ame@snapshot"

  /** Fixed tool identity for both roles; passing it in the environment keeps `commit-tree` working
    * in a repository with no `user.name`/`user.email` configured.
    */
  private val IdentityEnv = Map(
    "GIT_AUTHOR_NAME" -> Identity,
    "GIT_AUTHOR_EMAIL" -> IdentityEmail,
    "GIT_COMMITTER_NAME" -> Identity,
    "GIT_COMMITTER_EMAIL" -> IdentityEmail
  )

  /** Records the current state of the repository containing `dir` as `refs/ame/snapshots/<id>`.
    *
    * `dir` may be any directory inside the repository; the snapshot always covers the whole
    * repository, not just that subtree. An existing id is refused with
    * [[GitError.SnapshotExists]] unless `force` is set, which repoints the ref atomically.
    */
  def create(dir: String, id: String, force: Boolean = false): Either[GitError, SnapshotCreated] =
    for
      _        <- requireRepository(dir)
      ref      <- refFor(dir, id)
      _        <- requireAbsent(dir, ref, id, force)
      parent    = headCommit(dir)
      headTree  = parent.flatMap(commit => treeOf(dir, commit))
      tree     <- writeSnapshotTree(dir, parent)
      commit   <- commitTree(dir, tree, parent, id)
      // Without force, the empty old-value makes update-ref itself assert "must not exist yet":
      // requireAbsent gave the friendly error, this closes the race between the check and the write.
      _        <- if force then GitCmd.run(dir, Map.empty, "update-ref", ref, commit)
                  else GitCmd.run(dir, Map.empty, "update-ref", ref, commit, "")
    yield SnapshotCreated(id, ref, commit, tree, parent, headTree.contains(tree))

  /** Every snapshot in the repository containing `dir`, sorted by id. */
  def list(dir: String): Either[GitError, List[SnapshotEntry]] =
    for
      _      <- requireRepository(dir)
      output <- GitCmd.run(dir, Map.empty, "for-each-ref", s"--format=$ListFormat", RefPrefix)
    yield output.linesIterator.flatMap(parseEntry).toList.sortBy(_.id)

  /** The commit a snapshot points at, or [[GitError.SnapshotNotFound]]. */
  def resolve(dir: String, id: String): Either[GitError, String] =
    for
      _      <- requireRepository(dir)
      ref    <- refFor(dir, id)
      commit <- refCommit(dir, ref).toRight(GitError.SnapshotNotFound(id))
    yield commit

  /** Drops a snapshot's ref. The commit itself is left to git's own garbage collection. */
  def delete(dir: String, id: String): Either[GitError, Unit] =
    for
      _      <- requireRepository(dir)
      ref    <- refFor(dir, id)
      commit <- refCommit(dir, ref).toRight(GitError.SnapshotNotFound(id))
      _      <- GitCmd.run(dir, Map.empty, "update-ref", "-d", ref, commit)
    yield ()

  // -----------------------------------------------------------------------------------------
  // Steps
  // -----------------------------------------------------------------------------------------

  /** Tab separates the fields because git's own ref-name rules forbid control characters, so no
    * refname can contain one.
    */
  private val ListFormat = "%(objectname)%09%(refname)"

  private def requireRepository(dir: String): Either[GitError, Unit] =
    GitCmd.run(dir, Map.empty, "rev-parse", "--git-dir") match
      case Right(_) => Right(())
      case Left(_)  => Left(GitError.NotARepository(dir))

  /** Validation of the id is delegated to git itself: whatever `check-ref-format` accepts as the
    * tail of our namespace is a legal id, which keeps this in step with the version of git actually
    * installed instead of duplicating its rules. The empty id is caught here because
    * `refs/ame/snapshots/` would otherwise be rejected for the less helpful reason that it ends in a
    * slash.
    */
  private def refFor(dir: String, id: String): Either[GitError, String] =
    if id.isEmpty then Left(GitError.InvalidId(id, "id must not be empty"))
    else
      val ref = RefPrefix + id
      GitCmd.run(dir, Map.empty, "check-ref-format", ref) match
        case Right(_) => Right(ref)
        case Left(_)  => Left(GitError.InvalidId(id, s"git check-ref-format rejects '$ref'"))

  private def requireAbsent(
    dir: String,
    ref: String,
    id: String,
    force: Boolean
  ): Either[GitError, Unit] =
    if force || refCommit(dir, ref).isEmpty then Right(()) else Left(GitError.SnapshotExists(id))

  /** `None` for an unborn HEAD — a fresh repository with no commits yet, which yields a parentless
    * snapshot. A detached HEAD is not special: it resolves to the detached commit.
    */
  private def headCommit(dir: String): Option[String] = revParse(dir, "HEAD")

  private def treeOf(dir: String, commit: String): Option[String] =
    revParse(dir, s"$commit^{tree}")

  private def refCommit(dir: String, ref: String): Option[String] = revParse(dir, ref)

  /** `--verify --quiet` turns "no such thing" into a silent exit 1, so a missing ref is `None`
    * rather than an error. The repository itself has already been vouched for by the time this runs.
    */
  private def revParse(dir: String, revision: String): Option[String] =
    GitCmd.run(dir, Map.empty, "rev-parse", "--verify", "--quiet", revision).toOption.filter(_.nonEmpty)

  /** `core.splitIndex` is forced off for the temp index. With it on — and it is a config a user can
    * turn on globally — writing the index also drops a `sharedindex.*` file next to it, inside the
    * user's `.git`. The tree that comes out is the same either way, so this costs nothing and keeps
    * the promise that a snapshot writes nothing into the repository but its own ref.
    */
  private val PlainIndex = List("-c", "core.splitIndex=false")

  /** The five-step dance of S2, minus the ref update: fill a private index with HEAD, bring it up to
    * date with the working tree, and hash the result out as a tree. Nothing here can touch
    * `.git/index`, since `GIT_INDEX_FILE` redirects every one of these commands at the temp file.
    */
  private def writeSnapshotTree(dir: String, parent: Option[String]): Either[GitError, String] =
    withTempIndex { index =>
      val env = Map("GIT_INDEX_FILE" -> index)
      def onIndex(args: String*) = GitCmd.run(dir, env, (PlainIndex ++ args)*)
      for
        _    <- if parent.isDefined then onIndex("read-tree", "HEAD")
                else onIndex("read-tree", "--empty")
        _    <- onIndex("add", "-A")
        tree <- onIndex("write-tree")
      yield tree
    }

  private def commitTree(
    dir: String,
    tree: String,
    parent: Option[String],
    id: String
  ): Either[GitError, String] =
    val args =
      List("commit-tree", tree)
        ++ parent.toList.flatMap(commit => List("-p", commit))
        ++ List("-m", s"ame snapshot $id")
    GitCmd.run(dir, IdentityEnv, args*)

  // -----------------------------------------------------------------------------------------
  // Temp index
  // -----------------------------------------------------------------------------------------

  /** Recognizable on purpose: a leftover is a bug, and the name says whose. */
  private val IndexPrefix = "ame-index-"

  private var indexCounter = 0

  /** Runs `body` against a private index file and deletes it afterwards, on every path out —
    * success, `Left`, or an exception from the facades. The path is absolute so that the git
    * commands, which run with `cwd` set to a possibly nested directory, all resolve it identically.
    */
  private def withTempIndex[A](body: String => Either[GitError, A]): Either[GitError, A] =
    val index = allocateIndex()
    try body(index)
    finally removeIndex(index)

  private def allocateIndex(): String =
    indexCounter += 1
    val random = (js.Math.random() * 1e9).toInt
    NodePath.join(NodeOs.tmpdir(), s"$IndexPrefix${NodeProcess.pid}-$indexCounter-$random")

  /** The lock file goes too: git writes `<index>.lock` and renames it into place, so a command that
    * died midway can leave one behind next to the index we own.
    */
  private def removeIndex(index: String): Unit =
    val options = js.Dictionary.empty[js.Any]
    options("force") = true
    NodeFs.rmSync(index, options)
    NodeFs.rmSync(index + ".lock", options)

  private def parseEntry(line: String): Option[SnapshotEntry] =
    line.split('\t') match
      case Array(commit, ref) if ref.startsWith(RefPrefix) && commit.nonEmpty =>
        Some(SnapshotEntry(ref.drop(RefPrefix.length), commit))
      case _ => None
