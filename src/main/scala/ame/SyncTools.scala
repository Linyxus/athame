package ame

import mcp.{CallToolResult, Json, JsonSchemaObject, TextContent, Tool, ToolAnnotations}
import mcp.server.{ToolContext, ToolProvider}

/** athame's sync operations, as MCP tools.
  *
  * `dir` is fixed at construction because the server is per-repository: the process was started in
  * one, and nothing a client sends can point it at another.
  *
  * Every call goes through [[Runner.execute]], the same function the command line calls, so the
  * text a model reads is the text a user reads — including the `error: ` prefix on the failures.
  * Nothing here renders anything of its own, which is the point: two wordings of "no open
  * generation" would be two things to keep in step.
  */
final class SyncTools(dir: String) extends ToolProvider:

  def list(): Vector[Tool] = SyncTools.tools

  /** Run one tool.
    *
    * The two failure channels are used as the protocol means them. A sync that failed is a
    * *result* carrying `isError`, because the model has to read it to decide what to do next; the
    * `Left` below is for a name that is not in [[list]], which the session has already refused and
    * which therefore cannot arrive — it is what an impossible dispatch would look like, not a
    * check.
    *
    * [[ToolContext]] goes unused: [[Runner]] is synchronous and silent, so there is no progress to
    * report and nothing to log while a call is in flight.
    */
  def call(
    name: String,
    arguments: Option[Json.Obj],
    context: ToolContext
  ): Either[String, CallToolResult] =
    // An argument these tools do not take is refused rather than ignored: a model that invented a
    // `path` has a theory about how this server works, and silently dropping it leaves it intact.
    if arguments.exists(_.fields.nonEmpty) then Right(SyncTools.refuse(s"$name takes no arguments"))
    else
      SyncTools.commands.get(name) match
        case Some(command) => Right(SyncTools.report(Runner.execute(command, dir)))
        case None          => Left(s"no tool named '$name'")

object SyncTools:

  // -------------------------------------------------------------------------------------------
  // The registry
  //
  // One table, read two ways: `tools` is what `tools/list` answers and `commands` is what a call
  // dispatches on, so a tool that can be listed can be called and the two can never drift.
  // -------------------------------------------------------------------------------------------

  /** Closed-world and read-only: these read the repository the server was started in. */
  private val reads: ToolAnnotations = ToolAnnotations(None, Some(true), None, None, Some(false))

  /** Closed-world and recording: these write a snapshot into the repository's own ref namespace. */
  private val records: ToolAnnotations = ToolAnnotations(None, None, None, None, Some(false))

  /** Closed-world and destructive: what these drop — an open generation, or a snapshot already
    * recorded — is gone, and it cannot be recovered.
    */
  private val discards: ToolAnnotations = ToolAnnotations(None, None, Some(true), None, Some(false))

  private def tool(name: String, title: String, description: String, hints: ToolAnnotations): Tool =
    Tool(
      name = name,
      title = Some(title),
      icons = None,
      description = Some(description),
      inputSchema = JsonSchemaObject.empty,
      execution = None,
      outputSchema = None,
      annotations = Some(hints),
      meta = None
    )

  /** The seven, in the order `tools/list` reports them, each with the command it runs. */
  private val registry: Vector[(Tool, Command)] = Vector(
    tool(
      "sync_begin",
      "Begin sync generation",
      "Opens a new sync generation, recording a snapshot of the working tree as it stands before " +
        "the sync runs. Fails if a generation is already open, which has to be committed or " +
        "aborted first.",
      records
    ) -> Command.SyncBegin,
    tool(
      "sync_commit",
      "Commit sync generation",
      "Closes the open generation, recording a snapshot of the working tree as the sync left it, " +
        "and reports whether anything changed. Fails if no generation is open.",
      records
    ) -> Command.SyncCommit,
    tool(
      "sync_amend",
      "Amend sync generation",
      "Replaces the last completed generation's post-sync snapshot with the working tree as it " +
        "stands now, as if that sync had ended here. This is for correcting a completed sync after " +
        "the fact, once the corrective edits have been made. Fails while a generation is open, and " +
        "when no generation has been completed yet.",
      discards
    ) -> Command.SyncAmend,
    tool(
      "sync_abort",
      "Abort sync generation",
      "Discards the open generation and the snapshot it recorded, as if it had never begun. The " +
        "working tree is left exactly as it is; only the record goes.",
      discards
    ) -> Command.SyncAbort,
    tool(
      "sync_list",
      "List sync generations",
      "Lists every generation, oldest first, with its number and whether it is open, committed " +
        "with changes, or committed with none. Reads the repository and changes nothing.",
      reads
    ) -> Command.SyncList,
    tool(
      "sync_log",
      "Sync generation log",
      "Shows each generation in full, newest first: when it began, and the commit and tree of the " +
        "snapshots it recorded. Reads the repository and changes nothing.",
      reads
    ) -> Command.SyncLog,
    tool(
      "sync_diff",
      "Diff since last sync",
      "Shows what has changed in the working tree since the last completed generation's post-sync " +
        "snapshot, as a git patch. That drift is what a reconciling engine has to resolve; an open " +
        "generation is never the baseline, because it has no recorded after-state.",
      reads
    ) -> Command.SyncDiff
  )

  val tools: Vector[Tool] = registry.map((declared, _) => declared)

  private val commands: Map[String, Command] =
    registry.map((tool, command) => tool.name -> command).toMap

  // -------------------------------------------------------------------------------------------
  // Results
  // -------------------------------------------------------------------------------------------

  /** One [[Outcome]] as one text block.
    *
    * The trailing newline goes: it is what terminates the last line of a terminal's output, and a
    * content block is not a line. Everything inside the body keeps its own newlines.
    *
    * `isError` is present only when there is an error. Absent is what the schema means by success;
    * `false` would be a claim where none is needed.
    */
  private def report(outcome: Outcome): CallToolResult =
    if outcome.exit == 0 then result(outcome.out, None)
    else result(outcome.err, Some(true))

  private def refuse(message: String): CallToolResult = result(message, Some(true))

  private def result(text: String, isError: Option[Boolean]): CallToolResult =
    CallToolResult(None, Vector(TextContent(text.stripSuffix("\n"), None, None)), None, isError)
