package sync

/** Behavior tests for generations, organized by the rules they pin (G1-G9).
  *
  * The repositories are `git.TempRepo`s under the OS temp directory, so nothing here can reach
  * athame's own git state. What the snapshot layer already guarantees — zero disturbance, hidden
  * refs, unborn and detached HEAD — is not re-proven here beyond the single sanity check G9 asks
  * for; these tests are about generations.
  */
class SyncSuite extends munit.FunSuite:

  // -------------------------------------------------------------------------------------------
  // Fixtures
  // -------------------------------------------------------------------------------------------

  /** This suite creates snapshots, and snapshots leave a temp index in the OS temp directory while
    * they are being written. Suites run concurrently in separate processes, so keep those files out
    * of the shared directory that git.SnapshotSuite's hygiene tests inspect.
    */
  private var releaseTmpdir: () => Unit = () => ()

  override def beforeAll(): Unit = releaseTmpdir = PrivateTmpdir.claim()

  override def afterAll(): Unit = releaseTmpdir()

  private def withRepo(body: git.TempRepo => Unit): Unit =
    val repo = git.TempRepo.create()
    try body(repo)
    finally repo.dispose()

  private def seed(repo: git.TempRepo): String =
    repo.write("a.txt", "hello")
    repo.commit("init")

  private def ok[A](result: Either[SyncError, A])(using munit.Location): A =
    result match
      case Right(value) => value
      case Left(error)  => fail(s"expected success, got $error")

  /** Writes a snapshot id straight through the git layer, bypassing the rules under test — the
    * only way to build the layouts that are supposed to be impossible.
    */
  private def plant(repo: git.TempRepo, id: String)(using munit.Location): String =
    git.Snapshot.create(repo.dir, id) match
      case Right(snapshot) => snapshot.commit
      case Left(error)     => fail(s"could not plant snapshot '$id': $error")

  private def snapshotIds(repo: git.TempRepo)(using munit.Location): List[String] =
    git.Snapshot.list(repo.dir) match
      case Right(entries) => entries.map(_.id)
      case Left(error)    => fail(s"could not list snapshots: $error")

  private def assertCorrupt(result: Either[SyncError, ?], mentions: String*)(using
    munit.Location
  ): Unit =
    result match
      case Left(SyncError.CorruptState(details)) =>
        mentions.foreach(text => assert(details.contains(text), clue(details)))
      case other => fail(s"expected CorruptState mentioning ${mentions.mkString(", ")}, got $other")

  // -------------------------------------------------------------------------------------------
  // begin (G1, G4)
  // -------------------------------------------------------------------------------------------

  test("begin: a fresh repository starts at generation 1"):
    withRepo: repo =>
      seed(repo)
      val generation = ok(Sync.begin(repo.dir))
      assertEquals(generation.number, 1)
      assertEquals(generation.post, None)
      assert(generation.open)
      assertEquals(generation.changed, None)

  test("begin: the pre snapshot is a real ref carrying the reported commit and tree"):
    withRepo: repo =>
      seed(repo)
      val generation = ok(Sync.begin(repo.dir))
      assertEquals(snapshotIds(repo), List("sync/1/pre"))
      assertEquals(
        repo.git("rev-parse", "--verify", "refs/ame/snapshots/sync/1/pre"),
        generation.pre.commit
      )
      assertEquals(git.Snapshot.resolve(repo.dir, "sync/1/pre"), Right(generation.pre.commit))
      assertEquals(git.Snapshot.treeOf(repo.dir, generation.pre.commit), Right(generation.pre.tree))

  test("begin: a second begin while one is open is refused"):
    withRepo: repo =>
      seed(repo)
      ok(Sync.begin(repo.dir))
      assertEquals(Sync.begin(repo.dir), Left(SyncError.GenerationOpen(1)))

  test("begin: the refused begin leaves the state alone"):
    withRepo: repo =>
      seed(repo)
      val first = ok(Sync.begin(repo.dir))
      assertEquals(Sync.begin(repo.dir), Left(SyncError.GenerationOpen(1)))
      assertEquals(snapshotIds(repo), List("sync/1/pre"))
      assertEquals(Sync.current(repo.dir), Right(Some(first)))

  // -------------------------------------------------------------------------------------------
  // commit (G5)
  // -------------------------------------------------------------------------------------------

  test("commit: the post ref appears and the generation closes"):
    withRepo: repo =>
      seed(repo)
      ok(Sync.begin(repo.dir))
      val committed = ok(Sync.commit(repo.dir))
      assertEquals(committed.number, 1)
      assert(!committed.open)
      assertEquals(snapshotIds(repo), List("sync/1/post", "sync/1/pre"))
      assertEquals(
        git.Snapshot.resolve(repo.dir, "sync/1/post"),
        Right(committed.post.get.commit)
      )

  test("commit: the closed generation reports whether anything changed"):
    withRepo: repo =>
      seed(repo)
      val begun = ok(Sync.begin(repo.dir))
      val committed = ok(Sync.commit(repo.dir))
      assertEquals(committed.pre, begun.pre)
      assert(committed.changed.isDefined)

  test("commit: committing again reports nothing open"):
    withRepo: repo =>
      seed(repo)
      ok(Sync.begin(repo.dir))
      ok(Sync.commit(repo.dir))
      assertEquals(Sync.commit(repo.dir), Left(SyncError.NoOpenGeneration))

  test("commit: with nothing ever begun reports nothing open"):
    withRepo: repo =>
      seed(repo)
      assertEquals(Sync.commit(repo.dir), Left(SyncError.NoOpenGeneration))

  // -------------------------------------------------------------------------------------------
  // A full cycle (G1, G3)
  // -------------------------------------------------------------------------------------------

  test("cycle: an edit between begin and commit is recorded as a change"):
    withRepo: repo =>
      seed(repo)
      ok(Sync.begin(repo.dir))
      repo.write("a.txt", "edited by the sync")
      repo.write("added.txt", "new file")
      val committed = ok(Sync.commit(repo.dir))
      assertEquals(committed.changed, Some(true))
      assertNotEquals(committed.post.get.tree, committed.pre.tree)
      assertEquals(repo.show(committed.post.get.commit, "a.txt"), "edited by the sync")
      assertEquals(repo.show(committed.post.get.commit, "added.txt"), "new file")
      assertEquals(repo.show(committed.pre.commit, "a.txt"), "hello")

  test("cycle: a sync that changed nothing says so"):
    withRepo: repo =>
      seed(repo)
      ok(Sync.begin(repo.dir))
      val committed = ok(Sync.commit(repo.dir))
      assertEquals(committed.changed, Some(false))
      assertEquals(committed.post.get.tree, committed.pre.tree)

  // -------------------------------------------------------------------------------------------
  // Numbering (G1, G2)
  // -------------------------------------------------------------------------------------------

  test("numbers: consecutive cycles are numbered 1 then 2"):
    withRepo: repo =>
      seed(repo)
      assertEquals(ok(Sync.begin(repo.dir)).number, 1)
      assertEquals(ok(Sync.commit(repo.dir)).number, 1)
      assertEquals(ok(Sync.begin(repo.dir)).number, 2)
      assertEquals(ok(Sync.commit(repo.dir)).number, 2)

  test("numbers: an aborted generation's number is handed out again"):
    withRepo: repo =>
      seed(repo)
      ok(Sync.begin(repo.dir))
      ok(Sync.commit(repo.dir))
      ok(Sync.begin(repo.dir))
      ok(Sync.commit(repo.dir))
      assertEquals(ok(Sync.begin(repo.dir)).number, 3)
      assertEquals(Sync.abort(repo.dir), Right(3))
      assertEquals(ok(Sync.begin(repo.dir)).number, 3)

  // -------------------------------------------------------------------------------------------
  // list (G6, G7)
  // -------------------------------------------------------------------------------------------

  test("list: a fresh repository has no generations"):
    withRepo: repo =>
      seed(repo)
      assertEquals(Sync.list(repo.dir), Right(Nil))

  test("list: generations come back ascending, with the open one last"):
    withRepo: repo =>
      seed(repo)
      ok(Sync.begin(repo.dir))
      ok(Sync.commit(repo.dir))
      repo.write("a.txt", "second")
      ok(Sync.begin(repo.dir))
      ok(Sync.commit(repo.dir))
      ok(Sync.begin(repo.dir))
      val generations = ok(Sync.list(repo.dir))
      assertEquals(generations.map(_.number), List(1, 2, 3))
      assertEquals(generations.map(_.open), List(false, false, true))
      assertEquals(generations.map(_.changed), List(Some(false), Some(false), None))

  test("list: eleven generations are ordered numerically, not as strings"):
    withRepo: repo =>
      seed(repo)
      (1 to 11).foreach { _ =>
        ok(Sync.begin(repo.dir))
        ok(Sync.commit(repo.dir))
      }
      assertEquals(ok(Sync.list(repo.dir)).map(_.number), (1 to 11).toList)
      // The id sort the snapshot layer uses would have put 10 and 11 between 1 and 2.
      assertEquals(snapshotIds(repo).take(4), List("sync/1/post", "sync/1/pre", "sync/10/post", "sync/10/pre"))

  test("list: each generation carries its own snapshots"):
    withRepo: repo =>
      seed(repo)
      val first = ok(Sync.begin(repo.dir))
      repo.write("a.txt", "changed once")
      val firstClosed = ok(Sync.commit(repo.dir))
      val second = ok(Sync.begin(repo.dir))
      val generations = ok(Sync.list(repo.dir))
      assertEquals(generations, List(firstClosed, second))
      assertEquals(generations.head.pre, first.pre)
      assertEquals(generations.head.changed, Some(true))

  // -------------------------------------------------------------------------------------------
  // current (G2)
  // -------------------------------------------------------------------------------------------

  test("current: nothing is open on a fresh repository"):
    withRepo: repo =>
      seed(repo)
      assertEquals(Sync.current(repo.dir), Right(None))

  test("current: the open generation is the one begin returned"):
    withRepo: repo =>
      seed(repo)
      val begun = ok(Sync.begin(repo.dir))
      assertEquals(Sync.current(repo.dir), Right(Some(begun)))

  test("current: nothing is open after a commit"):
    withRepo: repo =>
      seed(repo)
      ok(Sync.begin(repo.dir))
      ok(Sync.commit(repo.dir))
      assertEquals(Sync.current(repo.dir), Right(None))

  test("current: nothing is open after an abort"):
    withRepo: repo =>
      seed(repo)
      ok(Sync.begin(repo.dir))
      ok(Sync.abort(repo.dir))
      assertEquals(Sync.current(repo.dir), Right(None))

  // -------------------------------------------------------------------------------------------
  // abort (G2, G5)
  // -------------------------------------------------------------------------------------------

  test("abort: returns the number and leaves no trace of the generation"):
    withRepo: repo =>
      seed(repo)
      ok(Sync.begin(repo.dir))
      assertEquals(Sync.abort(repo.dir), Right(1))
      assertEquals(snapshotIds(repo), Nil)
      assertEquals(Sync.list(repo.dir), Right(Nil))
      assertEquals(git.Snapshot.resolve(repo.dir, "sync/1/pre"), Left(git.GitError.SnapshotNotFound("sync/1/pre")))

  test("abort: a fresh begin works afterwards"):
    withRepo: repo =>
      seed(repo)
      ok(Sync.begin(repo.dir))
      ok(Sync.abort(repo.dir))
      val restarted = ok(Sync.begin(repo.dir))
      assertEquals(restarted.number, 1)
      assertEquals(ok(Sync.commit(repo.dir)).number, 1)

  test("abort: with nothing open reports nothing open"):
    withRepo: repo =>
      seed(repo)
      assertEquals(Sync.abort(repo.dir), Left(SyncError.NoOpenGeneration))
      ok(Sync.begin(repo.dir))
      ok(Sync.commit(repo.dir))
      assertEquals(Sync.abort(repo.dir), Left(SyncError.NoOpenGeneration))

  // -------------------------------------------------------------------------------------------
  // Impossible layouts (G6)
  // -------------------------------------------------------------------------------------------

  test("corrupt: a post with no pre is reported by list"):
    withRepo: repo =>
      seed(repo)
      plant(repo, "sync/7/post")
      assertCorrupt(Sync.list(repo.dir), "7")

  test("corrupt: a post with no pre is reported by begin"):
    withRepo: repo =>
      seed(repo)
      plant(repo, "sync/7/post")
      assertCorrupt(Sync.begin(repo.dir), "7")
      assertEquals(snapshotIds(repo), List("sync/7/post"), "begin must not write over a broken state")

  test("corrupt: a post with no pre is reported by current, commit and abort"):
    withRepo: repo =>
      seed(repo)
      plant(repo, "sync/7/post")
      assertCorrupt(Sync.current(repo.dir), "7")
      assertCorrupt(Sync.commit(repo.dir), "7")
      assertCorrupt(Sync.abort(repo.dir), "7")

  test("corrupt: two generations open at once are reported by commit"):
    withRepo: repo =>
      seed(repo)
      plant(repo, "sync/1/pre")
      plant(repo, "sync/2/pre")
      assertCorrupt(Sync.commit(repo.dir), "1", "2")

  test("corrupt: two generations open at once are reported by list, current, begin and abort"):
    withRepo: repo =>
      seed(repo)
      plant(repo, "sync/1/pre")
      plant(repo, "sync/2/pre")
      assertCorrupt(Sync.list(repo.dir), "1", "2")
      assertCorrupt(Sync.current(repo.dir), "1", "2")
      assertCorrupt(Sync.begin(repo.dir), "1", "2")
      assertCorrupt(Sync.abort(repo.dir), "1", "2")

  test("corrupt: a healthy open generation next to a committed one is not corrupt"):
    withRepo: repo =>
      seed(repo)
      plant(repo, "sync/1/pre")
      plant(repo, "sync/1/post")
      plant(repo, "sync/5/pre")
      assertEquals(ok(Sync.list(repo.dir)).map(_.number), List(1, 5))
      assertEquals(ok(Sync.current(repo.dir)).map(_.number), Some(5))

  // -------------------------------------------------------------------------------------------
  // Foreign ids (G6)
  // -------------------------------------------------------------------------------------------

  test("foreign: snapshots outside the convention are ignored"):
    withRepo: repo =>
      seed(repo)
      plant(repo, "manual/backup")
      plant(repo, "sync/notanumber/pre")
      assertEquals(Sync.list(repo.dir), Right(Nil))
      assertEquals(Sync.current(repo.dir), Right(None))
      assertEquals(ok(Sync.begin(repo.dir)).number, 1)
      assertEquals(ok(Sync.commit(repo.dir)).number, 1)

  test("foreign: near misses of the id convention are ignored too"):
    withRepo: repo =>
      seed(repo)
      plant(repo, "sync/0/pre")
      plant(repo, "sync/01/pre")
      plant(repo, "sync/2")
      plant(repo, "sync/3/middle")
      plant(repo, "sync/4/pre/extra")
      assertEquals(Sync.list(repo.dir), Right(Nil))
      // None of them shifted the numbering: the first real generation is still 1.
      assertEquals(ok(Sync.begin(repo.dir)).number, 1)

  test("foreign: a foreign snapshot survives a full cycle untouched"):
    withRepo: repo =>
      seed(repo)
      val backup = plant(repo, "manual/backup")
      ok(Sync.begin(repo.dir))
      ok(Sync.commit(repo.dir))
      assertEquals(git.Snapshot.resolve(repo.dir, "manual/backup"), Right(backup))

  // -------------------------------------------------------------------------------------------
  // Inherited guarantees (G9) and location
  // -------------------------------------------------------------------------------------------

  test("unborn HEAD: a repository with no commits still syncs"):
    withRepo: repo =>
      repo.write("a.txt", "hello")
      val begun = ok(Sync.begin(repo.dir))
      assertEquals(begun.number, 1)
      repo.write("a.txt", "edited")
      val committed = ok(Sync.commit(repo.dir))
      assertEquals(committed.changed, Some(true))
      assertEquals(ok(Sync.list(repo.dir)).map(_.open), List(false))
      assertEquals(repo.show(committed.post.get.commit, "a.txt"), "edited")

  test("sanity: a begin/commit cycle leaves the repository byte-identical"):
    withRepo: repo =>
      seed(repo)
      repo.write("staged.txt", "staged")
      repo.git("add", "staged.txt")
      repo.write("a.txt", "unstaged edit")
      repo.write("untracked.txt", "loose")
      val status = repo.status
      val staged = repo.git("diff", "--cached")
      val head = repo.head

      ok(Sync.begin(repo.dir))
      ok(Sync.commit(repo.dir))

      assertEquals(repo.status, status)
      assertEquals(repo.git("diff", "--cached"), staged)
      assertEquals(repo.head, head)

  test("location: every entry point reports a directory outside a repository"):
    TempDir.plain: dir =>
      val expected = SyncError.Git(git.GitError.NotARepository(dir))
      assertEquals(Sync.begin(dir), Left(expected))
      assertEquals(Sync.commit(dir), Left(expected))
      assertEquals(Sync.abort(dir), Left(expected))
      assertEquals(Sync.list(dir), Left(expected))
      assertEquals(Sync.current(dir), Left(expected))

  test("location: a generation can be driven from a subdirectory"):
    withRepo: repo =>
      seed(repo)
      repo.write("sub/deep/c.txt", "deep")
      val nested = repo.path("sub/deep")
      ok(Sync.begin(nested))
      repo.write("root.txt", "root")
      val committed = ok(Sync.commit(nested))
      assertEquals(committed.number, 1)
      assertEquals(committed.changed, Some(true))
      assertEquals(repo.show(committed.post.get.commit, "root.txt"), "root")
      assertEquals(ok(Sync.list(repo.dir)).map(_.number), List(1))
