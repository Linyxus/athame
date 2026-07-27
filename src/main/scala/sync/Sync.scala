package sync

import scala.annotation.tailrec

/** One snapshot as a generation refers to it: the commit, and the tree that commit records. */
final case class SnapshotRef(commit: String, tree: String)

/** One sync operation, bracketed by the state before it and the state after it.
  *
  * A generation is `open` between [[Sync.begin]] and [[Sync.commit]] — the "before" state is
  * recorded and the "after" state is not yet known — and committed once both halves exist.
  * [[changed]] answers the question the whole thing exists for: did the sync actually alter the
  * working tree? It is `None` while the generation is still open, because the answer is not in yet.
  */
final case class Generation(number: Int, pre: SnapshotRef, post: Option[SnapshotRef]):
  def open: Boolean = post.isEmpty

  /** Whether the working tree ended up different from how it started, once the generation is
    * closed. Trees are compared rather than diffed: equal tree OIDs mean identical content.
    */
  def changed: Option[Boolean] = post.map(after => pre.tree != after.tree)

/** Numbered generations of sync operations, recorded as pairs of snapshots.
  *
  * Generation `n` is stored as the two snapshots `sync/<n>/pre` and `sync/<n>/post`, which live at
  * `refs/ame/snapshots/sync/<n>/{pre,post}`. Those refs are the entire database: there is no state
  * file to fall out of step with them, no lock, and nothing to migrate. The next generation number
  * is one past the highest that exists, so an aborted generation's number is simply free again.
  *
  * The pre/post pairing is a naming convention and nothing more. Both snapshots are parented on
  * whatever HEAD was current when each was taken, exactly as [[git.Snapshot.create]] always does, so
  * a post is NOT a child of its pre and the two are not connected in the commit graph. Read the
  * relationship from the ids, never from parentage.
  *
  * At most one generation is open at a time, which [[begin]] enforces by scanning before it writes.
  * A layout that breaks that rule, or a post with no pre, can only come from something outside
  * athame writing into the namespace, and is reported as [[SyncError.CorruptState]] rather than
  * guessed around. Ids under `refs/ame/snapshots/` that are not generations — another tool's
  * snapshots, or a human's — are ignored without comment.
  *
  * Everything else follows from the snapshot layer: taking a generation disturbs nothing in the
  * repository, works with an unborn or detached HEAD, and stays invisible to `git branch`.
  */
object Sync:

  /** Starts a generation: records the current state as its pre-snapshot.
    *
    * Fails with [[SyncError.GenerationOpen]] if one is already open — closing it is the caller's
    * decision to make, not this one's. A concurrent process that wins the race to the same ref
    * surfaces as `Git(SnapshotExists)`; the loser never retries and never forces, since forcing
    * would destroy the other process's record of where it started.
    */
  def begin(dir: String): Either[SyncError, Generation] =
    for
      slots   <- scan(dir)
      _       <- slots.find(_.post.isEmpty).map(open => SyncError.GenerationOpen(open.number)).toLeft(())
      number   = slots.map(_.number).maxOption.getOrElse(0) + 1
      created <- create(dir, preId(number))
    yield Generation(number, SnapshotRef(created.commit, created.tree), None)

  /** Closes the open generation by recording the current state as its post-snapshot. */
  def commit(dir: String): Either[SyncError, Generation] =
    for
      open     <- openGeneration(dir)
      created  <- create(dir, postId(open.number))
    yield open.copy(post = Some(SnapshotRef(created.commit, created.tree)))

  /** Re-records the last completed generation's post-snapshot from the working tree as it stands
    * now, so that generation's story becomes "the sync ended here instead".
    *
    * This is `git commit --amend` for a generation, and it keeps that analogy's bargain: the
    * snapshot being replaced is discarded rather than kept as a revision, and the ref moves in one
    * step, so nothing ever observes the generation without a post. It exists for the sync that
    * landed somewhere the user did not want — the corrective edits are made first, and this is what
    * stops the record from claiming otherwise.
    *
    * An open generation is refused before anything else is considered: the answer there is to
    * commit or abort it, and the generation below it is not the one the caller meant. With nothing
    * ever completed there is nothing to re-record, which is [[SyncError.NoCompletedGeneration]].
    * Nothing else refuses — amending twice, or amending a tree that already matches the recorded
    * post, simply records again — and [[Generation.changed]] can flip in either direction, since it
    * is derived from the trees rather than stored.
    */
  def amend(dir: String): Either[SyncError, Generation] =
    for
      slots   <- scan(dir)
      _       <- slots.find(_.post.isEmpty).map(open => SyncError.GenerationOpen(open.number)).toLeft(())
      last    <- slots.filter(_.post.isDefined).lastOption.toRight(SyncError.NoCompletedGeneration)
      pre     <- snapshotRef(dir, last.pre)
      created <- replace(dir, postId(last.number))
    yield Generation(last.number, pre, Some(SnapshotRef(created.commit, created.tree)))

  /** Drops the open generation's pre-snapshot, leaving no trace of it, and returns its number —
    * which the next [[begin]] will hand out again.
    */
  def abort(dir: String): Either[SyncError, Int] =
    for
      slots <- scan(dir)
      open  <- slots.find(_.post.isEmpty).toRight(SyncError.NoOpenGeneration)
      _     <- git.Snapshot.delete(dir, preId(open.number)).left.map(SyncError.Git.apply)
    yield open.number

  /** Every generation in the repository, ascending by number. */
  def list(dir: String): Either[SyncError, List[Generation]] =
    scan(dir).flatMap(slots => traverse(slots)(hydrate(dir, _)))

  /** The newest generation that ran to completion, if any.
    *
    * An open generation is not a candidate: its post snapshot does not exist yet, so there is no
    * recorded "after" state to measure anything against. A caller asking this question is asking
    * where the last known-good synced state is, and mid-sync that is still the generation before.
    */
  def lastCompleted(dir: String): Either[SyncError, Option[Generation]] =
    for
      slots    <- scan(dir)
      newest    = slots.filter(_.post.isDefined).lastOption
      hydrated <- traverse(newest.toList)(hydrate(dir, _))
    yield hydrated.headOption

  /** The open generation, if there is one. */
  def current(dir: String): Either[SyncError, Option[Generation]] =
    for
      slots   <- scan(dir)
      open     = slots.find(_.post.isEmpty)
      hydrated <- traverse(open.toList)(hydrate(dir, _))
    yield hydrated.headOption

  // -----------------------------------------------------------------------------------------
  // Ids
  // -----------------------------------------------------------------------------------------

  private def preId(number: Int): String = s"sync/$number/pre"

  private def postId(number: Int): String = s"sync/$number/post"

  private enum Slot:
    case Pre, Post

  /** A generation as the refs describe it, before any tree lookups. */
  private final case class Slots(number: Int, pre: String, post: Option[String])

  // -----------------------------------------------------------------------------------------
  // Scanning
  // -----------------------------------------------------------------------------------------

  /** Reads the whole namespace and validates it. Every operation starts here, so every operation
    * refuses to act on a layout it does not understand.
    *
    * Only the commits are collected; trees cost a git invocation each and are looked up later, for
    * the generations the caller actually gets back.
    */
  private def scan(dir: String): Either[SyncError, List[Slots]] =
    for
      entries <- git.Snapshot.list(dir).left.map(SyncError.Git.apply)
      slots   <- group(entries.flatMap(parse))
    yield slots

  /** Ids that are not `sync/<n>/pre` or `sync/<n>/post` belong to somebody else and are dropped
    * here. The number must be a plain positive decimal: `sync/0/pre` is not a generation, and
    * neither is `sync/01/pre`, since nothing in this file ever writes a padded number and reading
    * one as generation 1 would collide with the real `sync/1/pre`.
    */
  private def parse(entry: git.SnapshotEntry): Option[(Int, Slot, String)] =
    entry.id.split('/') match
      case Array("sync", number, slot) =>
        for
          parsedNumber <- generationNumber(number)
          parsedSlot   <- slotOf(slot)
        yield (parsedNumber, parsedSlot, entry.commit)
      case _ => None

  private def generationNumber(text: String): Option[Int] =
    if text.isEmpty || text.exists(character => character < '0' || character > '9') then None
    else if text.length > 1 && text.startsWith("0") then None
    else text.toIntOption.filter(_ > 0)

  private def slotOf(text: String): Option[Slot] =
    text match
      case "pre"  => Some(Slot.Pre)
      case "post" => Some(Slot.Post)
      case _      => None

  /** Groups the parsed ids by number — ascending numerically, which is the whole point: sorting
    * these as strings would file generation 10 between 1 and 2 — and rejects the two layouts the
    * rules here cannot produce.
    */
  private def group(parsed: List[(Int, Slot, String)]): Either[SyncError, List[Slots]] =
    val byNumber = parsed.groupBy((number, _, _) => number)
    val found = byNumber.keys.toList.sorted.map { number =>
      val entries = byNumber(number)
      (
        number,
        entries.collectFirst { case (_, Slot.Pre, commit) => commit },
        entries.collectFirst { case (_, Slot.Post, commit) => commit }
      )
    }
    val orphan = found.collectFirst { case (number, None, Some(_)) => number }
    val generations = found.collect { case (number, Some(pre), post) => Slots(number, pre, post) }
    val open = generations.filter(_.post.isEmpty).map(_.number)

    if orphan.isDefined then
      Left(
        SyncError.CorruptState(
          s"generation ${orphan.get} has a post snapshot but no pre snapshot"
        )
      )
    else if open.sizeIs > 1 then
      Left(
        SyncError.CorruptState(
          s"generations ${open.mkString(", ")} are open at once; at most one may be"
        )
      )
    else Right(generations)

  // -----------------------------------------------------------------------------------------
  // Plumbing
  // -----------------------------------------------------------------------------------------

  private def openGeneration(dir: String): Either[SyncError, Generation] =
    for
      slots <- scan(dir)
      open  <- slots.find(_.post.isEmpty).toRight(SyncError.NoOpenGeneration)
      full  <- hydrate(dir, open)
    yield full

  /** Fills in the tree OIDs the refs alone do not carry. */
  private def hydrate(dir: String, slots: Slots): Either[SyncError, Generation] =
    for
      pre  <- snapshotRef(dir, slots.pre)
      post <- traverse(slots.post.toList)(snapshotRef(dir, _))
    yield Generation(slots.number, pre, post.headOption)

  private def snapshotRef(dir: String, commit: String): Either[SyncError, SnapshotRef] =
    git.Snapshot.treeOf(dir, commit).map(SnapshotRef(commit, _)).left.map(SyncError.Git.apply)

  private def create(dir: String, id: String): Either[SyncError, git.SnapshotCreated] =
    git.Snapshot.create(dir, id).left.map(SyncError.Git.apply)

  /** Creating over an id that already exists, which is only ever [[amend]] moving a post. Every
    * other caller goes through [[create]] and keeps the collision refusal that stops two
    * generations from sharing a ref.
    */
  private def replace(dir: String, id: String): Either[SyncError, git.SnapshotCreated] =
    git.Snapshot.create(dir, id, force = true).left.map(SyncError.Git.apply)

  /** Applies `f` across the list, stopping at the first failure so a broken repository does not
    * spawn a git process per remaining entry.
    */
  private def traverse[A, B](items: List[A])(f: A => Either[SyncError, B]): Either[SyncError, List[B]] =
    @tailrec
    def loop(remaining: List[A], done: List[B]): Either[SyncError, List[B]] =
      remaining match
        case Nil => Right(done.reverse)
        case head :: tail =>
          f(head) match
            case Right(value) => loop(tail, value :: done)
            case Left(error)  => Left(error)
    loop(items, Nil)
