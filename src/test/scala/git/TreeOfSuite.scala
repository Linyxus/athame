package git

/** Tests for [[Snapshot.treeOf]], the tree lookup layers above this one use to tell two recorded
  * states apart.
  */
class TreeOfSuite extends munit.FunSuite:

  /** Suites run concurrently in separate processes and snapshots leave a temp index in the OS temp
    * directory while they are written, so this one keeps its temp files somewhere private rather
    * than in the shared directory [[SnapshotSuite]]'s hygiene tests inspect. `os.tmpdir()` reads
    * `TMPDIR` on every call, so redirecting it moves this process's temp files and nobody else's.
    */
  private var releaseTmpdir: () => Unit = () => ()

  override def beforeAll(): Unit =
    val previous = NodeProcess.env.get("TMPDIR")
    val own = TestFs.mkdtempSync(TestPath.join(TestOs.tmpdir(), "ame-suite-"))
    NodeProcess.env("TMPDIR") = own
    releaseTmpdir = () =>
      previous match
        case Some(value) => NodeProcess.env("TMPDIR") = value
        case None        => NodeProcess.env -= "TMPDIR"
      val options = scala.scalajs.js.Dictionary.empty[scala.scalajs.js.Any]
      options("recursive") = true
      options("force") = true
      TestFs.rmSync(own, options)

  override def afterAll(): Unit = releaseTmpdir()

  private def withRepo(body: TempRepo => Unit): Unit =
    val repo = TempRepo.create()
    try body(repo)
    finally repo.dispose()

  private def created(result: Either[GitError, SnapshotCreated])(using
    munit.Location
  ): SnapshotCreated =
    result match
      case Right(snapshot) => snapshot
      case Left(error)     => fail(s"expected a snapshot, got $error")

  test("treeOf: a commit resolves to the tree it records"):
    withRepo: repo =>
      repo.write("a.txt", "hello")
      val head = repo.commit("init")
      assertEquals(Snapshot.treeOf(repo.dir, head), Right(repo.git("rev-parse", "HEAD^{tree}")))
      assertEquals(Snapshot.treeOf(repo.dir, "HEAD"), Right(repo.git("rev-parse", "HEAD^{tree}")))

  test("treeOf: a snapshot resolves by ref name or by commit to its own tree"):
    withRepo: repo =>
      repo.write("a.txt", "hello")
      repo.commit("init")
      repo.write("untracked.txt", "loose")
      val snapshot = created(Snapshot.create(repo.dir, "s1"))
      assertEquals(Snapshot.treeOf(repo.dir, snapshot.ref), Right(snapshot.tree))
      assertEquals(Snapshot.treeOf(repo.dir, snapshot.commit), Right(snapshot.tree))
      // The snapshot caught the untracked file, so its tree is not HEAD's.
      assertNotEquals(snapshot.tree, repo.git("rev-parse", "HEAD^{tree}"))

  test("treeOf: an unresolvable revision comes back as the failed git command"):
    withRepo: repo =>
      repo.write("a.txt", "hello")
      repo.commit("init")
      Snapshot.treeOf(repo.dir, "no-such-revision") match
        case Left(GitError.CommandFailed(args, exitCode, stderr)) =>
          assert(args.contains("rev-parse"), clue(args))
          assertNotEquals(exitCode, 0)
          assert(stderr.nonEmpty, "git was supposed to explain itself")
        case other => fail(s"expected CommandFailed, got $other")

  test("committedAt: a commit reports its committer date as ISO-8601"):
    withRepo: repo =>
      repo.write("a.txt", "hello")
      val head = repo.commit("init")
      Snapshot.committedAt(repo.dir, head) match
        case Right(date) =>
          assert(date.matches("""\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}.*"""), clue(date))
          assertEquals(date, repo.git("show", "-s", "--format=%cI", head))
        case other => fail(s"expected a timestamp, got $other")

  test("committedAt: a snapshot's ref name works as well as its commit"):
    withRepo: repo =>
      repo.write("a.txt", "hello")
      repo.commit("init")
      val snapshot = created(Snapshot.create(repo.dir, "s1"))
      assertEquals(
        Snapshot.committedAt(repo.dir, snapshot.ref),
        Snapshot.committedAt(repo.dir, snapshot.commit)
      )

  test("committedAt: an unresolvable revision comes back as the failed git command"):
    withRepo: repo =>
      repo.write("a.txt", "hello")
      repo.commit("init")
      Snapshot.committedAt(repo.dir, "no-such-revision") match
        case Left(GitError.CommandFailed(args, exitCode, stderr)) =>
          assert(args.contains("show"), clue(args))
          assertNotEquals(exitCode, 0)
          assert(stderr.nonEmpty, "git was supposed to explain itself")
        case other => fail(s"expected CommandFailed, got $other")

  test("treeOf: a directory outside a repository is not a repository"):
    val dir = TestFs.mkdtempSync(TestPath.join(TestOs.tmpdir(), "ame-plain-"))
    val options = scala.scalajs.js.Dictionary.empty[scala.scalajs.js.Any]
    options("recursive") = true
    options("force") = true
    try assertEquals(Snapshot.treeOf(dir, "HEAD"), Left(GitError.NotARepository(dir)))
    finally TestFs.rmSync(dir, options)
