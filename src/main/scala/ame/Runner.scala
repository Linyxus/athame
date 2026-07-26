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
      case Command.SyncAbort  => report(Sync.abort(dir).map(number => s"aborted generation $number"))
      case Command.SyncList   => report(Sync.list(dir).map(renderList))
      case Command.SyncLog    => report(Sync.list(dir).flatMap(renderLog(dir, _)))

  // -----------------------------------------------------------------------------------------
  // Rendering
  //
  // Renderers return a body with no trailing newline; `succeed` adds the one that terminates the
  // last line. Multi-line bodies join with "\n", so every line ends up newline-terminated exactly
  // once.
  // -----------------------------------------------------------------------------------------

  private def begun(generation: Generation): String = s"begun generation ${generation.number}"

  private def committed(generation: Generation): String =
    val summary = generation.changed match
      case Some(true) => "changed"
      case _          => "no changes"
    s"committed generation ${generation.number} ($summary)"

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

  private def report(result: Either[SyncError, String]): Outcome =
    result match
      case Right(body) => succeed(body)
      case Left(error) => Outcome("", s"error: ${message(error)}\n", 1)

  private def committedAt(dir: String, commit: String): Either[SyncError, String] =
    git.Snapshot.committedAt(dir, commit).left.map(SyncError.Git.apply)

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
