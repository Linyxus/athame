package mcp.server

import mcp.*

/** What a tool learns about the call it is serving, and what it may say while serving it.
  *
  * Both facilities emit through the session's sink the moment they are called, ahead of the reply
  * the tool has not produced yet. On stdio that is exactly what arrives on the wire; over HTTP the
  * ordering is looser, and the reason is worth knowing before writing a tool that depends on it —
  * see [[HttpServer]].
  */
trait ToolContext:
  /** The token the client attached to this call's `_meta`, if it asked for progress at all. */
  def progressToken: Option[ProgressToken]

  /** Report progress. Silently does nothing when the client did not ask for it: the notification
    * has nowhere to point without a token, so there is no message to send rather than a message to
    * suppress.
    */
  def progress(current: Double, total: Option[Double] = None, message: Option[String] = None): Unit

  /** Emit a log notification, subject to the level the client set with `logging/setLevel`. */
  def log(level: LoggingLevel, logger: Option[String], data: Json): Unit

/** The tools a server offers.
  *
  * The split between the two failure channels is the protocol's, not ours, and it matters:
  *
  *   - a tool that ran and failed returns `Right(CallToolResult(isError = Some(true)))`, because
  *     the model has to see what went wrong in order to correct itself;
  *   - `Left(message)` is the provider itself failing — a bug, a broken dependency — and becomes a
  *     JSON-RPC internal error, which the model never sees;
  *   - a name that is not in [[list]] never reaches [[call]]; the session answers that as a
  *     protocol error.
  */
trait ToolProvider:
  def list(): Vector[Tool]

  def call(
    name: String,
    arguments: Option[Json.Obj],
    context: ToolContext
  ): Either[String, CallToolResult]
