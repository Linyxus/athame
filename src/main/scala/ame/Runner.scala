package ame

import sync.{Generation, SnapshotRef, Sync, SyncError}

/** Everything one command produced: text destined for stdout, text destined for stderr, and the
  * exit code. A failure fills `err` and leaves `out` empty; a success does the reverse.
  */
final case class Outcome(out: String, err: String, exit: Int)

/** Runs a parsed [[Command]] and says what should be printed.
  *
  * Nothing here touches the process: no printing, no exit code, no reading of the working
  * directory — `dir` arrives as a parameter. That is what lets the tests drive the real logic
  * against real repositories without spawning a binary, and it keeps Main.scala small enough to
  * read in one go.
  *
  * Every string this produces is part of the command line's contract: the formats below are what
  * scripts will grep and what the tests pin exactly.
  */
object Runner:

  val version: String = "0.0.1-SNAPSHOT"

  def execute(command: Command, dir: String): Outcome =
    command match
      case Command.Version    => succeed(version)
      case Command.SyncBegin  => report(Sync.begin(dir).map(begun))
      case Command.SyncCommit => report(Sync.commit(dir).map(committed))
      case Command.SyncAmend  => report(Sync.amend(dir).map(amended))
      case Command.SyncAbort  => report(Sync.abort(dir).map(number => s"aborted generation $number"))
      case Command.SyncList   => report(Sync.list(dir).map(renderList))
      case Command.SyncLog    => report(Sync.list(dir).flatMap(renderLog(dir, _)))
      case Command.SyncDiff   => diff(dir)

  // -----------------------------------------------------------------------------------------
  // Rendering
  //
  // Renderers return a body with no trailing newline; `succeed` adds the one that terminates the
  // last line. Multi-line bodies join with "\n", so every line ends up newline-terminated exactly
  // once.
  // -----------------------------------------------------------------------------------------

  private def begun(generation: Generation): String = s"begun generation ${generation.number}"

  private def committed(generation: Generation): String =
    s"committed generation ${generation.number} (${summary(generation)})"

  /** Amend answers in commit's own shape, because it closes a generation the same way: the verb is
    * the only thing that differs, and the reader wants the same two facts out of both.
    */
  private def amended(generation: Generation): String =
    s"amended generation ${generation.number} (${summary(generation)})"

  private def summary(generation: Generation): String =
    generation.changed match
      case Some(true) => "changed"
      case _          => "no changes"

  private def renderList(generations: List[Generation]): String =
    if generations.isEmpty then "no generations"
    else generations.map(listLine).mkString("\n")

  private def listLine(generation: Generation): String =
    generation.changed match
      case None        => s"#${generation.number}  open"
      case Some(true)  => s"#${generation.number}  committed  changed"
      case Some(false) => s"#${generation.number}  committed  no-op"

  /** Newest first: a log is read from the top, and the interesting generation is the last one. */
  private def renderLog(dir: String, generations: List[Generation]): Either[SyncError, String] =
    if generations.isEmpty then Right("no generations")
    else traverse(generations.reverse)(logBlock(dir, _)).map(_.mkString("\n\n"))

  private def logBlock(dir: String, generation: Generation): Either[SyncError, String] =
    committedAt(dir, generation.pre.commit).map { begunAt =>
      val header = generation.changed match
        case None        => s"generation ${generation.number} (open)"
        case Some(true)  => s"generation ${generation.number} (committed, changed)"
        case Some(false) => s"generation ${generation.number} (committed, no changes)"
      val lines =
        List(header, s"  begun: $begunAt", s"  pre:  ${describe(generation.pre)}")
          ++ generation.post.map(post => s"  post: ${describe(post)}")
      lines.mkString("\n")
    }

  private def describe(snapshot: SnapshotRef): String =
    s"${snapshot.commit} tree ${snapshot.tree}"

  // -----------------------------------------------------------------------------------------
  // diff
  // -----------------------------------------------------------------------------------------

  /** What has drifted since the last completed sync.
    *
    * The baseline is that generation's post snapshot, and an open generation is never a baseline:
    * it has no recorded "after" state. Running this mid-sync therefore shows the human's edits
    * together with whatever the sync has done so far, both measured from the last state the two
    * sides agreed on — which is the view a reconciling engine needs.
    */
  private def diff(dir: String): Outcome =
    Sync.lastCompleted(dir) match
      case Left(error)                                    => failed(error)
      case Right(Some(Generation(number, _, Some(post)))) => renderDiff(dir, number, post.tree)
      case Right(_)                                       => failedWith("no completed generation to diff against")

  private def renderDiff(dir: String, number: Int, baseline: String): Outcome =
    val patch =
      for
        target <- fromGit(git.Snapshot.currentTree(dir))
        text   <- fromGit(git.Snapshot.diffTrees(dir, baseline, target))
      yield text
    patch match
      case Left(error) => failed(error)
      case Right("")   => succeed(s"no changes since generation $number")
      // git's own bytes, newline-terminated already: passed through rather than reformatted.
      case Right(text) => Outcome(text, "", 0)

  // -----------------------------------------------------------------------------------------
  // Failure
  // -----------------------------------------------------------------------------------------

  /** One line per failure, prefixed `error: `. The git cases are spelled out because a user can
    * act on them; anything else prints the error as it stands rather than inventing prose for a
    * case the sync layer cannot produce.
    */
  private def message(error: SyncError): String =
    error match
      case SyncError.GenerationOpen(generation) =>
        s"generation $generation is already open (commit or abort it first)"
      case SyncError.NoOpenGeneration =>
        "no open generation"
      case SyncError.NoCompletedGeneration =>
        "no completed generation to amend"
      case SyncError.CorruptState(details) =>
        s"corrupt sync state: $details"
      case SyncError.Git(git.GitError.NotARepository(dir)) =>
        s"not a git repository: $dir"
      case SyncError.Git(git.GitError.CommandFailed(_, exitCode, stderr)) =>
        s"git failed (exit $exitCode): ${stderr.trim}"
      case SyncError.Git(other) =>
        other.toString

  // -----------------------------------------------------------------------------------------
  // Plumbing
  // -----------------------------------------------------------------------------------------

  private def succeed(body: String): Outcome = Outcome(body + "\n", "", 0)

  private def failedWith(text: String): Outcome = Outcome("", s"error: $text\n", 1)

  private def failed(error: SyncError): Outcome = failedWith(message(error))

  private def report(result: Either[SyncError, String]): Outcome =
    result match
      case Right(body) => succeed(body)
      case Left(error) => failed(error)

  private def fromGit[A](result: Either[git.GitError, A]): Either[SyncError, A] =
    result.left.map(SyncError.Git.apply)

  private def committedAt(dir: String, commit: String): Either[SyncError, String] =
    fromGit(git.Snapshot.committedAt(dir, commit))

  private def traverse[A, B](items: List[A])(f: A => Either[SyncError, B]): Either[SyncError, List[B]] =
    @annotation.tailrec
    def loop(remaining: List[A], done: List[B]): Either[SyncError, List[B]] =
      remaining match
        case Nil => Right(done.reverse)
        case head :: tail =>
          f(head) match
            case Right(value) => loop(tail, value :: done)
            case Left(error)  => Left(error)
    loop(items, Nil)
