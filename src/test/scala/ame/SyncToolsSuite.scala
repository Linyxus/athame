package ame

import mcp.{CallToolResult, Codec, Json, Path, ServerMessage}
import sync.{PrivateTmpdir, TempDir}

import scala.collection.mutable

/** The tools, driven the way a client drives them: whole JSON-RPC messages in, whole replies out,
  * over a real session and a real repository.
  *
  * Two contracts are pinned here. The `tools/list` reply is the first: it is what every model that
  * ever talks to this server reads before it decides what to call, so it is asserted byte for byte.
  * The second is that a tool's text *is* the command line's text — asserted against
  * [[Runner.execute]] on the same repository wherever the output is not worth spelling out, since
  * the point is the equality and not the wording.
  */
class SyncToolsSuite extends munit.FunSuite:

  /** Snapshots leave a temp index in the OS temp directory while they are written, and suites run
    * concurrently in separate processes; keep this suite's out of the shared one.
    */
  private var releaseTmpdir: () => Unit = () => ()

  override def beforeAll(): Unit = releaseTmpdir = PrivateTmpdir.claim()

  override def afterAll(): Unit = releaseTmpdir()

  // -------------------------------------------------------------------------------------------
  // Fixtures
  // -------------------------------------------------------------------------------------------

  private val initializeRequest =
    """{"jsonrpc":"2.0","id":0,"method":"initialize","params":{"protocolVersion":"2025-11-25","capabilities":{},"clientInfo":{"name":"test-client","version":"1.0.0"}}}"""

  private val initializedNotification =
    """{"jsonrpc":"2.0","method":"notifications/initialized"}"""

  /** A client on the other end of a session built exactly as `ame serve` builds it.
    *
    * Request ids are handed out in order and the handshake takes id 0, so the first tool call in a
    * test is id 1 and a test reads as the conversation it is.
    */
  private final class Client(dir: String):
    private val sent = mutable.ArrayBuffer.empty[String]
    private var lastId = 0

    val session: mcp.server.ServerSession = Serve.build(dir)(text => sent.append(text))

    /** Everything the server said on its own initiative. */
    def sink: Vector[String] = sent.toVector

    def ask(message: String)(using munit.Location): String =
      session.receive(message) match
        case Vector(reply) => reply
        case other =>
          fail(s"expected exactly one reply, got ${other.length}: ${other.mkString(" | ")}")

    def ready()(using munit.Location): Client =
      ask(initializeRequest)
      assertEquals(session.receive(initializedNotification), Vector.empty[String])
      this

    def listTools()(using munit.Location): String = ask(request("tools/list", None))

    def call(name: String, arguments: Option[String] = None)(using munit.Location): String =
      val carried = arguments.map(text => s""","arguments":$text""").getOrElse("")
      ask(request("tools/call", Some(s"""{"name":"$name"$carried}""")))

    private def request(method: String, params: Option[String]): String =
      lastId += 1
      val tail = params.map(text => s""","params":$text""").getOrElse("")
      s"""{"jsonrpc":"2.0","id":$lastId,"method":"$method"$tail}"""

  private def withRepo(body: git.TempRepo => Unit): Unit =
    val repo = git.TempRepo.create()
    try body(repo)
    finally repo.dispose()

  /** A repository with one commit in it, and a client already through the handshake. */
  private def withClient(body: (git.TempRepo, Client) => Unit)(using munit.Location): Unit =
    withRepo: repo =>
      repo.write("a.txt", "hello")
      repo.commit("init")
      body(repo, Client(repo.dir).ready())

  private def block(text: String): String = s"""{"type":"text","text":${Codec.quote(text)}}"""

  /** The reply a successful call earns: one text block, and no `isError` member at all. */
  private def success(id: Int, text: String): String =
    s"""{"jsonrpc":"2.0","id":$id,"result":{"content":[${block(text)}]}}"""

  /** The reply a failed call earns: a result, not a protocol error, so the model can read it. */
  private def failure(id: Int, text: String): String =
    s"""{"jsonrpc":"2.0","id":$id,"result":{"content":[${block(text)}],"isError":true}}"""

  private def resultOf(reply: String)(using munit.Location): CallToolResult =
    Codec.decodeServerMessage(reply) match
      case Right(ServerMessage.Response(_, payload)) =>
        CallToolResult.decoder.decode(Path.root, Json.toJs(payload)) match
          case Right(result) => result
          case Left(error)   => fail(s"a tool result did not decode: ${error.render}")
      case other => fail(s"expected a response, got $other")

  // -------------------------------------------------------------------------------------------
  // tools/list
  //
  // Written out by hand rather than composed from the values under test: this is the description
  // of athame that every client reads, and a golden derived from the encoder would agree with
  // whatever the encoder happened to do.
  // -------------------------------------------------------------------------------------------

  private val beginTool =
    """{"name":"sync_begin","title":"Begin sync generation","description":"Opens a new sync generation, recording a snapshot of the working tree as it stands before the sync runs. Fails if a generation is already open, which has to be committed or aborted first.","inputSchema":{"type":"object"},"annotations":{"openWorldHint":false}}"""

  private val commitTool =
    """{"name":"sync_commit","title":"Commit sync generation","description":"Closes the open generation, recording a snapshot of the working tree as the sync left it, and reports whether anything changed. Fails if no generation is open.","inputSchema":{"type":"object"},"annotations":{"openWorldHint":false}}"""

  private val abortTool =
    """{"name":"sync_abort","title":"Abort sync generation","description":"Discards the open generation and the snapshot it recorded, as if it had never begun. The working tree is left exactly as it is; only the record goes.","inputSchema":{"type":"object"},"annotations":{"destructiveHint":true,"openWorldHint":false}}"""

  private val listTool =
    """{"name":"sync_list","title":"List sync generations","description":"Lists every generation, oldest first, with its number and whether it is open, committed with changes, or committed with none. Reads the repository and changes nothing.","inputSchema":{"type":"object"},"annotations":{"readOnlyHint":true,"openWorldHint":false}}"""

  private val logTool =
    """{"name":"sync_log","title":"Sync generation log","description":"Shows each generation in full, newest first: when it began, and the commit and tree of the snapshots it recorded. Reads the repository and changes nothing.","inputSchema":{"type":"object"},"annotations":{"readOnlyHint":true,"openWorldHint":false}}"""

  private val diffTool =
    """{"name":"sync_diff","title":"Diff since last sync","description":"Shows what has changed in the working tree since the last completed generation's post-sync snapshot, as a git patch. That drift is what a reconciling engine has to resolve; an open generation is never the baseline, because it has no recorded after-state.","inputSchema":{"type":"object"},"annotations":{"readOnlyHint":true,"openWorldHint":false}}"""

  private def toolsListReply(id: Int) =
    s"""{"jsonrpc":"2.0","id":$id,"result":{"tools":[$beginTool,$commitTool,$abortTool,$listTool,$logTool,$diffTool]}}"""

  test("tools/list: the whole reply, byte for byte — this is what a model reads"):
    withClient: (_, client) =>
      assertEquals(client.listTools(), toolsListReply(1))

  test("tools/list: the answer does not depend on the repository's state"):
    withClient: (_, client) =>
      client.call("sync_begin")
      client.call("sync_commit")
      assertEquals(client.listTools(), toolsListReply(3))

  // -------------------------------------------------------------------------------------------
  // A generation, through the tools alone
  // -------------------------------------------------------------------------------------------

  test("scenario: begin and commit run a generation and report what they did"):
    withClient: (repo, client) =>
      assertEquals(client.call("sync_begin"), success(1, "begun generation 1"))
      assertEquals(client.call("sync_commit"), success(2, "committed generation 1 (no changes)"))
      assertEquals(client.call("sync_begin"), success(3, "begun generation 2"))
      repo.write("a.txt", "edited by the sync")
      assertEquals(client.call("sync_commit"), success(4, "committed generation 2 (changed)"))

  test("scenario: abort takes the open generation away again"):
    withClient: (_, client) =>
      assertEquals(client.call("sync_begin"), success(1, "begun generation 1"))
      assertEquals(client.call("sync_abort"), success(2, "aborted generation 1"))
      assertEquals(client.call("sync_begin"), success(3, "begun generation 1"))

  // -------------------------------------------------------------------------------------------
  // The text is the command line's text
  // -------------------------------------------------------------------------------------------

  test("list: the block is the command line's stdout, minus the terminator"):
    withClient: (repo, client) =>
      client.call("sync_begin")
      client.call("sync_commit")
      client.call("sync_begin")
      val cli = Runner.execute(Command.SyncList, repo.dir)
      assert(cli.out.endsWith("\n"), clue(cli.out))
      assertEquals(client.call("sync_list"), success(4, cli.out.stripSuffix("\n")))
      assertEquals(cli.out, "#1  committed  no-op\n#2  open\n")

  test("log: a multi-line block keeps every newline but the last"):
    withClient: (repo, client) =>
      client.call("sync_begin")
      client.call("sync_commit")
      val cli = Runner.execute(Command.SyncLog, repo.dir)
      val text = resultOf(client.call("sync_log")).content.head match
        case block: mcp.TextContent => block.text
        case other                  => fail(s"expected a text block, got $other")
      assertEquals(text, cli.out.stripSuffix("\n"))
      assert(text.contains("\n"), clue(text))
      assert(!text.endsWith("\n"), clue(text))

  test("diff: a patch travels as one block, escaping and all"):
    withClient: (repo, client) =>
      client.call("sync_begin")
      client.call("sync_commit")
      repo.write("a.txt", "edited by hand")
      val cli = Runner.execute(Command.SyncDiff, repo.dir)
      assert(cli.out.contains("+edited by hand"), clue(cli.out))
      assertEquals(client.call("sync_diff"), success(3, cli.out.stripSuffix("\n")))

  test("diff: with nothing to diff against, the command line's error is the tool's error"):
    withClient: (repo, client) =>
      val cli = Runner.execute(Command.SyncDiff, repo.dir)
      assertEquals(cli.exit, 1)
      assertEquals(client.call("sync_diff"), failure(1, cli.err.stripSuffix("\n")))

  // -------------------------------------------------------------------------------------------
  // Shape of a result
  // -------------------------------------------------------------------------------------------

  test("success: isError is absent, not false, and there is exactly one block"):
    withClient: (_, client) =>
      val reply = client.call("sync_list")
      assert(!reply.contains("isError"), clue(reply))
      val result = resultOf(reply)
      assertEquals(result.isError, None)
      assertEquals(result.content.length, 1)
      assertEquals(result.structuredContent, None)
      assertEquals(result.meta, None)

  test("failure: isError says so, and the error prose is the command line's"):
    withClient: (_, client) =>
      val result = resultOf(client.call("sync_commit"))
      assertEquals(result.isError, Some(true))
      assertEquals(result.content.length, 1)
      assertEquals(result.meta, None)

  test("tools: nothing is ever said through the sink"):
    withClient: (_, client) =>
      client.listTools()
      client.call("sync_begin")
      client.call("sync_commit")
      client.call("sync_log")
      assertEquals(client.sink, Vector.empty[String])

  // -------------------------------------------------------------------------------------------
  // Failures the model has to see
  // -------------------------------------------------------------------------------------------

  test("errors: committing with nothing open"):
    withClient: (_, client) =>
      assertEquals(client.call("sync_commit"), failure(1, "error: no open generation"))

  test("errors: aborting with nothing open"):
    withClient: (_, client) =>
      assertEquals(client.call("sync_abort"), failure(1, "error: no open generation"))

  test("errors: beginning twice says what to do about the first"):
    withClient: (_, client) =>
      client.call("sync_begin")
      assertEquals(
        client.call("sync_begin"),
        failure(2, "error: generation 1 is already open (commit or abort it first)")
      )

  test("errors: a directory that is not a repository"):
    TempDir.plain: dir =>
      val client = Client(dir).ready()
      assertEquals(client.call("sync_list"), failure(1, s"error: not a git repository: $dir"))
      assertEquals(client.call("sync_begin"), failure(2, s"error: not a git repository: $dir"))

  // -------------------------------------------------------------------------------------------
  // Arguments
  // -------------------------------------------------------------------------------------------

  test("arguments: an absent object and an empty one both run the tool"):
    withClient: (_, client) =>
      assertEquals(client.call("sync_list", None), success(1, "no generations"))
      assertEquals(client.call("sync_list", Some("{}")), success(2, "no generations"))

  test("arguments: an invented argument is refused rather than ignored"):
    withClient: (_, client) =>
      assertEquals(
        client.call("sync_begin", Some("""{"path":"/somewhere/else"}""")),
        failure(1, "sync_begin takes no arguments")
      )
      // Refused means refused: no generation was begun by the call that complained.
      assertEquals(client.call("sync_list"), success(2, "no generations"))

  test("arguments: the complaint names the tool the model called"):
    withClient: (_, client) =>
      assertEquals(
        client.call("sync_diff", Some("""{"since":1}""")),
        failure(1, "sync_diff takes no arguments")
      )

  // -------------------------------------------------------------------------------------------
  // Boundary: what the session answers rather than the provider
  // -------------------------------------------------------------------------------------------

  test("dispatch: a tool that does not exist is the session's protocol error, not a result"):
    withClient: (_, client) =>
      assertEquals(
        client.call("sync_frobnicate"),
        """{"jsonrpc":"2.0","id":1,"error":{"code":-32602,"message":"Unknown tool: sync_frobnicate"}}"""
      )
