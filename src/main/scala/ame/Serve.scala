package ame

import mcp.{Implementation, ServerCapabilities}
import mcp.server.{HttpServer, ServerSession, Stdio}

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport

/** The little of `node:process` this file needs. Everything else in athame stays away from the
  * process, which is what lets the rest of `serve` be tested without one.
  */
@js.native
private trait ProcessStream extends js.Object:
  def write(chunk: String): Boolean = js.native

@js.native
@JSImport("node:process", JSImport.Default)
private object NodeProcess extends js.Object:
  def cwd(): String = js.native
  def stderr: ProcessStream = js.native
  var exitCode: Int = js.native

/** How `ame serve` was asked to run, exactly as the argument vector spelled it.
  *
  * The three flags are kept raw rather than reconciled during the parse: `--host` without `--http`
  * is a well-formed argument vector describing an incoherent request, and the parser's job is to
  * report what was written. [[mode]] is where it is judged, which is also what makes that judgement
  * a table of cases a test can read rather than a branch inside a server that has already started.
  */
final case class ServeConfig(http: Boolean, host: Option[String], port: Option[Int]):

  def mode: Either[String, ServeMode] =
    if http then
      Right(ServeMode.Http(host.getOrElse(ServeConfig.DefaultHost), port.getOrElse(ServeConfig.DefaultPort)))
    else if host.isDefined || port.isDefined then Left("--host and --port only apply with --http")
    else Right(ServeMode.Stdio)

object ServeConfig:
  /** Loopback, because the bind address *is* the HTTP transport's security boundary: it has no TLS
    * and no authentication, and binding anywhere else needs both.
    */
  val DefaultHost: String = "127.0.0.1"

  val DefaultPort: Int = 3000

/** Which transport a validated [[ServeConfig]] asked for, with every default already resolved. */
enum ServeMode:
  case Stdio
  case Http(host: String, port: Int)

/** `ame serve`: athame's sync operations, offered to a model over MCP.
  *
  * The session is assembled here and executed by [[SyncTools]], which runs the same [[Runner]] the
  * command line does — so a tool's text is the CLI's text, byte for byte, and there is one place
  * where each of these operations is worded.
  *
  * ==Out of scope, deliberately==
  *
  *   - A repository argument per call. The server is per-repository by construction: [[start]]
  *     fixes the directory at startup and every session over that process shares it. A model that
  *     could name a path could name any path.
  *   - `outputSchema` and `structuredContent`. The results are prose meant for a model to read.
  *   - Progress and log notifications from tools: [[Runner]] is synchronous and says nothing until
  *     it is done, so there is nothing to report while a call is in flight.
  *   - Tool-list pagination and `notifications/tools/list_changed`: six tools, fixed for the life
  *     of the process.
  *   - Signal handling. There is no state to flush on the way out, so Node's default exit is the
  *     whole shutdown story.
  *   - TLS, authorization and origin policy, all of which belong to the transport and are what
  *     [[ServeConfig.DefaultHost]] stands in for.
  *   - Sampling and elicitation, and exposing anything that is not a sync operation: `version` is
  *     a thing a user asks a binary, not a thing a model needs to call.
  */
object Serve:

  /** What a client is told the tools are for, in the `initialize` result.
    *
    * Addressed to whoever is driving the model: the generation model is the part that cannot be
    * inferred from six tool descriptions read separately.
    */
  val Instructions: String =
    "athame records what a sync did to a repository, as a numbered series of generations. " +
      "Begin a generation before the sync writes anything and commit it once the sync is done: " +
      "begin snapshots the working tree as the sync found it, commit snapshots it as the sync " +
      "left it, and only one generation may be open at a time. Abort discards an open generation " +
      "without recording anything; the working tree is never touched by any of these. Use list " +
      "and log to inspect the series, and diff to see what has changed since the last completed " +
      "generation. Every tool acts on the one repository this server was started in and takes no " +
      "arguments."

  val serverInfo: Implementation =
    Implementation(
      name = "athame",
      title = Some("athame"),
      icons = None,
      version = Runner.version,
      description = None,
      websiteUrl = None
    )

  /** A session factory for `dir`, which is what both transports want: the sink a session writes its
    * server-initiated traffic to is the transport's, and it does not exist until the transport has
    * a session to give it to.
    *
    * Nothing but `tools` is declared. The session derives that one from the provider, and no other
    * capability has anything behind it — a server that advertised logging would be promising
    * notifications nothing here emits.
    */
  def build(dir: String): (String => Unit) => ServerSession =
    sink => ServerSession(serverInfo, ServerCapabilities.empty, Some(SyncTools(dir)), sink, Some(Instructions))

  /** Start serving, and hand the process over to Node's event loop.
    *
    * The only part of `serve` that touches the process, and it is kept to that: everything it
    * decides was decided by [[ServeConfig.mode]] before it was called.
    */
  def start(config: ServeConfig): Unit =
    config.mode match
      case Left(message) =>
        NodeProcess.stderr.write(s"error: $message\n")
        NodeProcess.exitCode = 1

      // Not a word to stdout or stderr: on stdio, stdout is the wire, and a banner on it is a
      // corrupted stream rather than a friendly greeting.
      case Right(ServeMode.Stdio) =>
        Stdio.serve(build(NodeProcess.cwd()))

      // Under HTTP stdout is free, and the bound port is the one thing a user cannot work out for
      // themselves — so it is the whole of the startup output.
      case Right(ServeMode.Http(host, port)) =>
        HttpServer(build(NodeProcess.cwd()), host, port)
          .start(bound => println(s"listening on http://$host:$bound/mcp"))
