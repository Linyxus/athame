package ame

import mcp.{ServerCapabilities, ToolsCapability}

/** What `ame serve` decided before it touched the process, and what it hands a client once it has.
  *
  * [[Serve.start]] itself is not exercised here — it binds a socket or takes over stdin, which is
  * the one thing this codebase does not test — so everything that decides *what* it will do lives
  * in [[ServeConfig.mode]] and [[Serve.build]], both of which are ordinary values.
  */
class ServeSuite extends munit.FunSuite:

  // -------------------------------------------------------------------------------------------
  // Which transport was asked for
  // -------------------------------------------------------------------------------------------

  test("mode: no flags at all is stdio"):
    assertEquals(ServeConfig(false, None, None).mode, Right(ServeMode.Stdio))

  test("mode: --http alone takes both defaults"):
    assertEquals(ServeConfig(true, None, None).mode, Right(ServeMode.Http("127.0.0.1", 3000)))

  test("mode: --http fills in whichever half was left out"):
    assertEquals(ServeConfig(true, Some("0.0.0.0"), None).mode, Right(ServeMode.Http("0.0.0.0", 3000)))
    assertEquals(ServeConfig(true, None, Some(9001)).mode, Right(ServeMode.Http("127.0.0.1", 9001)))

  test("mode: --http with both takes both"):
    assertEquals(
      ServeConfig(true, Some("0.0.0.0"), Some(9001)).mode,
      Right(ServeMode.Http("0.0.0.0", 9001))
    )

  /** Silently ignoring an address that cannot take effect would leave a user believing they had
    * bound one. The message names both options because either alone is the same mistake.
    */
  test("mode: an address or a port without --http is a usage error"):
    val complaint = Left("--host and --port only apply with --http")
    assertEquals(ServeConfig(false, Some("0.0.0.0"), None).mode, complaint)
    assertEquals(ServeConfig(false, None, Some(9001)).mode, complaint)
    assertEquals(ServeConfig(false, Some("0.0.0.0"), Some(9001)).mode, complaint)

  // -------------------------------------------------------------------------------------------
  // What a session says for itself
  // -------------------------------------------------------------------------------------------

  private val initializeRequest =
    """{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2025-11-25","capabilities":{},"clientInfo":{"name":"test-client","version":"1.0.0"}}}"""

  /** The handshake in full: athame's identity, the one capability it declares, and the paragraph
    * that tells a client how six tools fit together. Building a session reads nothing from disk, so
    * a directory that does not exist is the honest fixture for "this decides nothing about git".
    */
  private val initializeReply =
    """{"jsonrpc":"2.0","id":1,"result":{"protocolVersion":"2025-11-25","capabilities":{"tools":{}},"serverInfo":{"name":"athame","title":"athame","version":"0.0.1-SNAPSHOT"},"instructions":"athame records what a sync did to a repository, as a numbered series of generations. Begin a generation before the sync writes anything and commit it once the sync is done: begin snapshots the working tree as the sync found it, commit snapshots it as the sync left it, and only one generation may be open at a time. Abort discards an open generation without recording anything; the working tree is never touched by any of these. Use list and log to inspect the series, and diff to see what has changed since the last completed generation. Every tool acts on the one repository this server was started in and takes no arguments."}}"""

  private def session() = Serve.build("/definitely/not/a/repository")(_ => ())

  test("build: the handshake, byte for byte"):
    assertEquals(session().receive(initializeRequest), Vector(initializeReply))

  test("build: tools is the only capability declared"):
    // Derived from the provider by the session; nothing else has anything behind it, and a server
    // that declared logging would be promising notifications no tool here ever sends.
    assertEquals(
      session().capabilities,
      ServerCapabilities.empty.copy(tools = Some(ToolsCapability(None)))
    )

  test("build: the server names itself and carries the binary's version"):
    assertEquals(session().serverInfo.name, "athame")
    assertEquals(session().serverInfo.version, Runner.version)

  test("build: each call makes its own session, so HTTP clients never share one"):
    val factory = Serve.build("/definitely/not/a/repository")
    assertNotEquals(factory(_ => ()), factory(_ => ()))
