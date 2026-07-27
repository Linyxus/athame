package sync

/** Failures produced while working with generations.
  *
  * [[Git]] wraps anything the underlying snapshot layer reports, including
  * `GitError.NotARepository` for a directory outside a repository and `GitError.SnapshotExists`
  * when a concurrent process won a creation race.
  *
  * [[GenerationOpen]], [[NoOpenGeneration]] and [[NoCompletedGeneration]] are ordinary "wrong
  * moment" answers — a caller can see them by asking for the wrong thing in the right repository.
  * [[CorruptState]] is different: the refs describe a layout the rules here cannot produce, so
  * something outside athame edited the namespace. It carries a description rather than structure
  * because there is nothing a caller can do with it except show it to a human.
  */
enum SyncError:
  case Git(error: git.GitError)
  case GenerationOpen(generation: Int)
  case NoOpenGeneration
  case NoCompletedGeneration
  case CorruptState(details: String)
