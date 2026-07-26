package mcp.server

/** Newline framing (V4).
  *
  * A stream chops its data wherever it likes, so every test here is about a boundary landing
  * somewhere inconvenient.
  */
class FramesSuite extends ServerSuite:

  test("frames: a whole line in one chunk comes straight out"):
    val frames = LineFrames()
    assertEquals(frames.push("{\"a\":1}\n"), Vector("{\"a\":1}"))
    assertEquals(frames.pending, "")

  test("frames: a line split across chunks is held until it is complete"):
    val frames = LineFrames()
    assertEquals(frames.push("{\"a\""), Vector.empty[String])
    assertEquals(frames.pending, "{\"a\"")
    assertEquals(frames.push(":1"), Vector.empty[String])
    assertEquals(frames.push("}\n"), Vector("{\"a\":1}"))
    assertEquals(frames.pending, "")

  test("frames: a chunk split one character at a time still reassembles"):
    val frames = LineFrames()
    val message = """{"jsonrpc":"2.0","id":1,"method":"ping"}"""
    val out = (message + "\n").map(character => frames.push(character.toString)).flatten
    assertEquals(out.toVector, Vector(message))

  test("frames: several lines in one chunk all come out, in order"):
    val frames = LineFrames()
    assertEquals(frames.push("a\nb\nc\n"), Vector("a", "b", "c"))
    assertEquals(frames.pending, "")

  test("frames: a chunk ending mid-line yields what is complete and carries the rest"):
    val frames = LineFrames()
    assertEquals(frames.push("a\nb\nc"), Vector("a", "b"))
    assertEquals(frames.pending, "c")
    assertEquals(frames.push("d\n"), Vector("cd"))

  test("frames: empty lines are skipped rather than passed on as empty messages"):
    val frames = LineFrames()
    assertEquals(frames.push("\n\n"), Vector.empty[String])
    assertEquals(frames.push("a\n\n\nb\n"), Vector("a", "b"))

  test("frames: a line of blanks is not empty and goes through"):
    // It will fail to parse, which is the honest outcome: someone sent something that was not a
    // message, and they should hear about it rather than have it vanish.
    val frames = LineFrames()
    assertEquals(frames.push("   \n"), Vector("   "))

  test("frames: CRLF is tolerated, because Windows peers exist"):
    val frames = LineFrames()
    assertEquals(frames.push("a\r\nb\r\n"), Vector("a", "b"))

  test("frames: a CRLF split between the two characters still works"):
    val frames = LineFrames()
    assertEquals(frames.push("a\r"), Vector.empty[String])
    assertEquals(frames.push("\n"), Vector("a"))

  test("frames: only one carriage return is stripped"):
    // Two in a row are not a line ending anyone meant, so the second stays and becomes a parse
    // error rather than being quietly swallowed.
    val frames = LineFrames()
    assertEquals(frames.push("a\r\r\n"), Vector("a\r"))

  test("frames: nothing is emitted for a chunk with no newline in it at all"):
    val frames = LineFrames()
    assertEquals(frames.push("no newline here"), Vector.empty[String])
    assertEquals(frames.pending, "no newline here")

  test("frames: an empty chunk changes nothing"):
    val frames = LineFrames()
    assertEquals(frames.push("partial"), Vector.empty[String])
    assertEquals(frames.push(""), Vector.empty[String])
    assertEquals(frames.pending, "partial")

  test("frames: flush releases a final unterminated line"):
    val frames = LineFrames()
    assertEquals(frames.push("{\"a\":1}"), Vector.empty[String])
    assertEquals(frames.flush(), Vector("{\"a\":1}"))
    assertEquals(frames.pending, "")

  test("frames: flush on a clean boundary releases nothing"):
    val frames = LineFrames()
    assertEquals(frames.push("a\n"), Vector("a"))
    assertEquals(frames.flush(), Vector.empty[String])

  test("frames: flush twice is not a way to get the same line twice"):
    val frames = LineFrames()
    frames.push("a")
    assertEquals(frames.flush(), Vector("a"))
    assertEquals(frames.flush(), Vector.empty[String])

  test("frames: flush strips a trailing carriage return too"):
    val frames = LineFrames()
    frames.push("a\r")
    assertEquals(frames.flush(), Vector("a"))

  test("frames: a JSON message containing an escaped newline is still one line"):
    // JSON.stringify escapes newlines inside strings, which is the whole reason newline framing is
    // safe for this protocol. The escaped form has no raw \n in it.
    val encoded = mcp.Json.stringify(mcp.Json.Str("two\nlines"))
    assert(!encoded.contains('\n'), encoded)
    val frames = LineFrames()
    assertEquals(frames.push(encoded + "\n"), Vector(encoded))
