package ame

import sync.{PrivateTmpdir, TempDir}

/** Executor tests: the real sync logic against real repositories, asserting the whole [[Outcome]].
  *
  * Every string here is pinned exactly, because these are the strings the command line promises.
  * Timestamps are the one exception — they are matched by shape, since they cannot be predicted.
  */
class RunnerSuite extends munit.FunSuite:

  /** Snapshots leave a temp index in the OS temp directory while they are written, and suites run
    * concurrently in separate processes; keep this suite's out of the shared one.
    */
  private var releaseTmpdir: () => Unit = () => ()

  override def beforeAll(): Unit = releaseTmpdir = PrivateTmpdir.claim()

  override def afterAll(): Unit = releaseTmpdir()

  // -------------------------------------------------------------------------------------------
  // Fixtures
  // -------------------------------------------------------------------------------------------

  private def withRepo(body: git.TempRepo => Unit): Unit =
    val repo = git.TempRepo.create()
    try body(repo)
    finally repo.dispose()

  private def seed(repo: git.TempRepo): String =
    repo.write("a.txt", "hello")
    repo.commit("init")

  private def run(repo: git.TempRepo, command: Command): Outcome =
    Runner.execute(command, repo.dir)

  private def plant(repo: git.TempRepo, id: String)(using munit.Location): Unit =
    git.Snapshot.create(repo.dir, id) match
      case Right(_)    => ()
      case Left(error) => fail(s"could not plant snapshot '$id': $error")

  /** The commit and tree git reports for a snapshot id, read independently of the runner. */
  private def snapshotOf(repo: git.TempRepo, id: String)(using munit.Location): (String, String) =
    val commit = git.Snapshot.resolve(repo.dir, id) match
      case Right(value) => value
      case Left(error)  => fail(s"could not resolve '$id': $error")
    git.Snapshot.treeOf(repo.dir, commit) match
      case Right(tree) => (commit, tree)
      case Left(error) => fail(s"could not read the tree of '$id': $error")

  private val isoTimestamp = """\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}.*"""

  // -------------------------------------------------------------------------------------------
  // version
  // -------------------------------------------------------------------------------------------

  test("version: prints the version and needs no repository"):
    assertEquals(
      Runner.execute(Command.Version, "/definitely/not/a/repository"),
      Outcome("0.0.1-SNAPSHOT\n", "", 0)
    )

  // -------------------------------------------------------------------------------------------
  // begin
  // -------------------------------------------------------------------------------------------

  test("begin: reports the new generation and records it"):
    withRepo: repo =>
      seed(repo)
      assertEquals(run(repo, Command.SyncBegin), Outcome("begun generation 1\n", "", 0))
      assert(git.Snapshot.resolve(repo.dir, "sync/1/pre").isRight)

  test("begin: a second begin says what to do about the first"):
    withRepo: repo =>
      seed(repo)
      run(repo, Command.SyncBegin)
      assertEquals(
        run(repo, Command.SyncBegin),
        Outcome("", "error: generation 1 is already open (commit or abort it first)\n", 1)
      )

  // -------------------------------------------------------------------------------------------
  // commit
  // -------------------------------------------------------------------------------------------

  test("commit: an edited working tree is reported as changed"):
    withRepo: repo =>
      seed(repo)
      run(repo, Command.SyncBegin)
      repo.write("a.txt", "edited")
      assertEquals(run(repo, Command.SyncCommit), Outcome("committed generation 1 (changed)\n", "", 0))

  test("commit: an untouched working tree is reported as no changes"):
    withRepo: repo =>
      seed(repo)
      run(repo, Command.SyncBegin)
      assertEquals(
        run(repo, Command.SyncCommit),
        Outcome("committed generation 1 (no changes)\n", "", 0)
      )

  test("commit: with nothing open says so"):
    withRepo: repo =>
      seed(repo)
      assertEquals(run(repo, Command.SyncCommit), Outcome("", "error: no open generation\n", 1))

  // -------------------------------------------------------------------------------------------
  // abort
  // -------------------------------------------------------------------------------------------

  test("abort: reports the number and drops the snapshot"):
    withRepo: repo =>
      seed(repo)
      run(repo, Command.SyncBegin)
      assertEquals(run(repo, Command.SyncAbort), Outcome("aborted generation 1\n", "", 0))
      assert(git.Snapshot.resolve(repo.dir, "sync/1/pre").isLeft)

  test("abort: with nothing open says so"):
    withRepo: repo =>
      seed(repo)
      assertEquals(run(repo, Command.SyncAbort), Outcome("", "error: no open generation\n", 1))

  // -------------------------------------------------------------------------------------------
  // list
  // -------------------------------------------------------------------------------------------

  test("list: a repository with no generations says so"):
    withRepo: repo =>
      seed(repo)
      assertEquals(run(repo, Command.SyncList), Outcome("no generations\n", "", 0))

  test("list: open, changed and no-op generations each render their own way"):
    withRepo: repo =>
      seed(repo)
      run(repo, Command.SyncBegin)
      repo.write("a.txt", "edited")
      run(repo, Command.SyncCommit)
      run(repo, Command.SyncBegin)
      run(repo, Command.SyncCommit)
      run(repo, Command.SyncBegin)
      assertEquals(
        run(repo, Command.SyncList),
        Outcome("#1  committed  changed\n#2  committed  no-op\n#3  open\n", "", 0)
      )

  test("list: more than nine generations are ordered numerically"):
    withRepo: repo =>
      seed(repo)
      (1 to 11).foreach { _ =>
        run(repo, Command.SyncBegin)
        run(repo, Command.SyncCommit)
      }
      val expected = (1 to 11).map(number => s"#$number  committed  no-op\n").mkString
      assertEquals(run(repo, Command.SyncList), Outcome(expected, "", 0))

  // -------------------------------------------------------------------------------------------
  // log
  // -------------------------------------------------------------------------------------------

  test("log: a repository with no generations says so"):
    withRepo: repo =>
      seed(repo)
      assertEquals(run(repo, Command.SyncLog), Outcome("no generations\n", "", 0))

  test("log: an open generation shows when it began and its pre snapshot"):
    withRepo: repo =>
      seed(repo)
      run(repo, Command.SyncBegin)
      val (commit, tree) = snapshotOf(repo, "sync/1/pre")
      val outcome = run(repo, Command.SyncLog)
      assertEquals(outcome.err, "")
      assertEquals(outcome.exit, 0)
      assert(outcome.out.endsWith("\n"), clue(outcome.out))
      val lines = outcome.out.linesIterator.toList
      assertEquals(lines.length, 3)
      assertEquals(lines(0), "generation 1 (open)")
      assert(lines(1).matches(s"  begun: $isoTimestamp"), clue(lines(1)))
      assertEquals(lines(2), s"  pre:  $commit tree $tree")

  test("log: a committed generation adds its post snapshot"):
    withRepo: repo =>
      seed(repo)
      run(repo, Command.SyncBegin)
      repo.write("a.txt", "edited")
      run(repo, Command.SyncCommit)
      val (preCommit, preTree) = snapshotOf(repo, "sync/1/pre")
      val (postCommit, postTree) = snapshotOf(repo, "sync/1/post")
      val lines = run(repo, Command.SyncLog).out.linesIterator.toList
      assertEquals(lines.length, 4)
      assertEquals(lines(0), "generation 1 (committed, changed)")
      assert(lines(1).matches(s"  begun: $isoTimestamp"), clue(lines(1)))
      assertEquals(lines(2), s"  pre:  $preCommit tree $preTree")
      assertEquals(lines(3), s"  post: $postCommit tree $postTree")

  test("log: a generation that changed nothing says so in its header"):
    withRepo: repo =>
      seed(repo)
      run(repo, Command.SyncBegin)
      run(repo, Command.SyncCommit)
      val lines = run(repo, Command.SyncLog).out.linesIterator.toList
      assertEquals(lines(0), "generation 1 (committed, no changes)")

  test("log: blocks run newest first, separated by a blank line"):
    withRepo: repo =>
      seed(repo)
      run(repo, Command.SyncBegin)
      run(repo, Command.SyncCommit)
      run(repo, Command.SyncBegin)
      val outcome = run(repo, Command.SyncLog)
      val lines = outcome.out.linesIterator.toList
      assertEquals(lines.length, 8)
      assertEquals(lines(0), "generation 2 (open)")
      assertEquals(lines(3), "")
      assertEquals(lines(4), "generation 1 (committed, no changes)")
      assert(lines(7).startsWith("  post: "), clue(lines(7)))

  // -------------------------------------------------------------------------------------------
  // diff
  // -------------------------------------------------------------------------------------------

  /** Drives one committed generation into the repository and leaves `a.txt` holding `content`. */
  private def committedGeneration(repo: git.TempRepo, content: String): Unit =
    run(repo, Command.SyncBegin)
    repo.write("a.txt", content)
    run(repo, Command.SyncCommit)

  test("diff: with no generations there is nothing to diff against"):
    withRepo: repo =>
      seed(repo)
      assertEquals(
        run(repo, Command.SyncDiff),
        Outcome("", "error: no completed generation to diff against\n", 1)
      )

  test("diff: an open generation alone is still nothing to diff against"):
    withRepo: repo =>
      seed(repo)
      run(repo, Command.SyncBegin)
      assertEquals(
        run(repo, Command.SyncDiff),
        Outcome("", "error: no completed generation to diff against\n", 1)
      )

  test("diff: an untouched working tree has no changes"):
    withRepo: repo =>
      seed(repo)
      run(repo, Command.SyncBegin)
      run(repo, Command.SyncCommit)
      assertEquals(run(repo, Command.SyncDiff), Outcome("no changes since generation 1\n", "", 0))

  test("diff: an edited tracked file shows its before and after"):
    withRepo: repo =>
      seed(repo)
      run(repo, Command.SyncBegin)
      run(repo, Command.SyncCommit)
      repo.write("a.txt", "edited by hand")
      val outcome = run(repo, Command.SyncDiff)
      assertEquals(outcome.err, "")
      assertEquals(outcome.exit, 0)
      assert(outcome.out.contains("diff --git a/a.txt b/a.txt"), clue(outcome.out))
      assert(outcome.out.contains("-hello"), clue(outcome.out))
      assert(outcome.out.contains("+edited by hand"), clue(outcome.out))
      assert(outcome.out.endsWith("\n"), clue(outcome.out))

  test("diff: a new untracked file shows up as an addition"):
    withRepo: repo =>
      seed(repo)
      run(repo, Command.SyncBegin)
      run(repo, Command.SyncCommit)
      repo.write("added.txt", "brand new")
      val outcome = run(repo, Command.SyncDiff)
      assertEquals(outcome.exit, 0)
      assert(outcome.out.contains("diff --git a/added.txt b/added.txt"), clue(outcome.out))
      assert(outcome.out.contains("new file mode"), clue(outcome.out))
      assert(outcome.out.contains("+brand new"), clue(outcome.out))

  test("diff: a deleted tracked file shows up as a deletion"):
    withRepo: repo =>
      seed(repo)
      run(repo, Command.SyncBegin)
      run(repo, Command.SyncCommit)
      repo.remove("a.txt")
      val outcome = run(repo, Command.SyncDiff)
      assertEquals(outcome.exit, 0)
      assert(outcome.out.contains("diff --git a/a.txt b/a.txt"), clue(outcome.out))
      assert(outcome.out.contains("deleted file mode"), clue(outcome.out))
      assert(outcome.out.contains("-hello"), clue(outcome.out))

  test("diff: a change to an ignored file is not drift"):
    withRepo: repo =>
      repo.write(".gitignore", "ignored.txt\n")
      repo.write("a.txt", "hello")
      repo.commit("init")
      run(repo, Command.SyncBegin)
      run(repo, Command.SyncCommit)
      repo.write("ignored.txt", "noise")
      assertEquals(run(repo, Command.SyncDiff), Outcome("no changes since generation 1\n", "", 0))

  test("diff: the baseline is the newest committed generation, not the first"):
    withRepo: repo =>
      seed(repo)
      committedGeneration(repo, "first sync")
      committedGeneration(repo, "second sync")
      repo.write("a.txt", "human edit")
      val outcome = run(repo, Command.SyncDiff)
      assertEquals(outcome.err, "")
      assertEquals(outcome.exit, 0)
      assert(outcome.out.contains("-second sync"), clue(outcome.out))
      assert(outcome.out.contains("+human edit"), clue(outcome.out))
      // Generation 1's content would appear as the removed side if it were the baseline.
      assert(!outcome.out.contains("first sync"), clue(outcome.out))

  test("diff: an open generation on top does not become the baseline"):
    withRepo: repo =>
      seed(repo)
      committedGeneration(repo, "first sync")
      committedGeneration(repo, "second sync")
      run(repo, Command.SyncBegin)
      repo.write("a.txt", "work in progress")
      val outcome = run(repo, Command.SyncDiff)
      assertEquals(outcome.exit, 0)
      assert(outcome.out.contains("-second sync"), clue(outcome.out))
      assert(outcome.out.contains("+work in progress"), clue(outcome.out))
      assert(!outcome.out.contains("first sync"), clue(outcome.out))

  test("diff: the no-changes line names the last committed generation"):
    withRepo: repo =>
      seed(repo)
      committedGeneration(repo, "first sync")
      committedGeneration(repo, "second sync")
      assertEquals(run(repo, Command.SyncDiff), Outcome("no changes since generation 2\n", "", 0))

  test("diff: corruption is reported rather than diffed"):
    withRepo: repo =>
      seed(repo)
      run(repo, Command.SyncBegin)
      run(repo, Command.SyncCommit)
      plant(repo, "sync/7/post")
      val outcome = run(repo, Command.SyncDiff)
      assertEquals(outcome.out, "")
      assertEquals(outcome.exit, 1)
      assert(outcome.err.startsWith("error: corrupt sync state: "), clue(outcome.err))
      assert(outcome.err.contains("7"), clue(outcome.err))

  // -------------------------------------------------------------------------------------------
  // Failures
  // -------------------------------------------------------------------------------------------

  test("location: every sync command reports a directory outside a repository"):
    TempDir.plain: dir =>
      val expected = Outcome("", s"error: not a git repository: $dir\n", 1)
      assertEquals(Runner.execute(Command.SyncBegin, dir), expected)
      assertEquals(Runner.execute(Command.SyncCommit, dir), expected)
      assertEquals(Runner.execute(Command.SyncAbort, dir), expected)
      assertEquals(Runner.execute(Command.SyncList, dir), expected)
      assertEquals(Runner.execute(Command.SyncLog, dir), expected)
      assertEquals(Runner.execute(Command.SyncDiff, dir), expected)

  test("corrupt: list explains what is broken"):
    withRepo: repo =>
      seed(repo)
      plant(repo, "sync/7/post")
      val outcome = run(repo, Command.SyncList)
      assertEquals(outcome.out, "")
      assertEquals(outcome.exit, 1)
      assert(outcome.err.startsWith("error: corrupt sync state: "), clue(outcome.err))
      assert(outcome.err.contains("7"), clue(outcome.err))
      assert(outcome.err.endsWith("\n"), clue(outcome.err))

  test("corrupt: begin refuses rather than writing into a broken state"):
    withRepo: repo =>
      seed(repo)
      plant(repo, "sync/7/post")
      val outcome = run(repo, Command.SyncBegin)
      assertEquals(outcome.out, "")
      assertEquals(outcome.exit, 1)
      assert(outcome.err.startsWith("error: corrupt sync state: "), clue(outcome.err))

  test("errors: a failing git command is reported with its exit code and complaint"):
    assume(
      scala.scalajs.js.Dynamic.global.process.platform.asInstanceOf[String] != "win32",
      "the fixture relies on POSIX permission bits"
    )
    withRepo: repo =>
      seed(repo)
      repo.write("unreadable.txt", "secret")
      repo.chmod("unreadable.txt", 0)
      val outcome = run(repo, Command.SyncBegin)
      assertEquals(outcome.out, "")
      assertEquals(outcome.exit, 1)
      assert(outcome.err.startsWith("error: git failed (exit "), clue(outcome.err))
      assert(outcome.err.contains("Permission denied"), clue(outcome.err))
      repo.chmod("unreadable.txt", Integer.parseInt("644", 8))

  // -------------------------------------------------------------------------------------------
  // Outcome discipline
  // -------------------------------------------------------------------------------------------

  test("discipline: successful commands say nothing on stderr"):
    withRepo: repo =>
      seed(repo)
      val successes = List(
        run(repo, Command.SyncList),
        run(repo, Command.SyncLog),
        run(repo, Command.SyncBegin),
        run(repo, Command.SyncCommit),
        run(repo, Command.SyncLog),
        run(repo, Command.SyncDiff),
        Runner.execute(Command.Version, repo.dir)
      )
      successes.foreach { outcome =>
        assertEquals(outcome.err, "", clue(outcome))
        assertEquals(outcome.exit, 0, clue(outcome))
        assert(outcome.out.endsWith("\n"), clue(outcome))
      }

  test("discipline: failing commands say nothing on stdout"):
    withRepo: repo =>
      seed(repo)
      val failures = List(
        run(repo, Command.SyncCommit),
        run(repo, Command.SyncAbort),
        run(repo, Command.SyncDiff),
        Runner.execute(Command.SyncList, "/definitely/not/a/repository")
      )
      failures.foreach { outcome =>
        assertEquals(outcome.out, "", clue(outcome))
        assertEquals(outcome.exit, 1, clue(outcome))
        assert(outcome.err.startsWith("error: "), clue(outcome))
        assert(outcome.err.endsWith("\n"), clue(outcome))
      }
