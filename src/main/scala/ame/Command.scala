package ame

/** What the argument vector asked for, once parsing has succeeded.
  *
  * The parser's job ends here: a `Command` names an action and carries nothing else, so deciding
  * what to do and doing it stay separate. [[Runner.execute]] is the only thing that interprets one.
  * Options and positionals do not exist in v1; when they arrive they belong in the cases here, as
  * parameters, and the CLI tree gains them at the matching `sub`.
  */
enum Command:
  case Version, SyncBegin, SyncCommit, SyncAmend, SyncAbort, SyncList, SyncLog, SyncDiff
