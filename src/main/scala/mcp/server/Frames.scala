package mcp.server

/** Newline framing for the stdio transport.
  *
  * A stream hands us bytes in whatever sizes it feels like: half a message, three messages, a
  * message split through the middle of a UTF-8 character. The first two are this class's problem;
  * the third is Node's, and setting the stream encoding to `utf8` is what makes it so — its decoder
  * carries a split character across chunk boundaries, which is why the adapter feeds us `String`
  * rather than `Buffer`.
  *
  * Mutable and single-threaded by design, because a stream is. Nothing here parses JSON: a line is
  * handed on exactly as it arrived and the session decides whether it is a message.
  */
final class LineFrames:

  /** The tail of the last chunk, after its final newline. */
  private var carry: String = ""

  /** The complete lines in `chunk`, given everything pushed before it.
    *
    * Empty lines are skipped rather than passed on as empty messages — a peer that writes `\n\n`
    * between messages is being untidy, not sending a malformed one. A line of blanks is *not*
    * empty and does go through, where it becomes an honest parse error rather than a silent drop.
    */
  def push(chunk: String): Vector[String] =
    val text = carry + chunk
    val lines = Vector.newBuilder[String]
    var start = 0
    var newline = text.indexOf('\n', start)
    while newline >= 0 do
      val line = trimReturn(text.substring(start, newline))
      if line.nonEmpty then lines += line
      start = newline + 1
      newline = text.indexOf('\n', start)
    carry = text.substring(start)
    lines.result()

  /** The last line if the stream ended without a final newline, and nothing otherwise.
    *
    * Peers are supposed to terminate every message, and most do; one that closes the pipe on an
    * unterminated line has still told us what it wanted to say.
    */
  def flush(): Vector[String] =
    val line = trimReturn(carry)
    carry = ""
    if line.isEmpty then Vector.empty else Vector(line)

  /** What is buffered and not yet a complete line. For tests, and for anyone debugging a stall. */
  def pending: String = carry

  /** Windows peers exist, and `\r\n` is a line ending there. One `\r` is stripped, not a run of
    * them: two in a row are not a line ending anybody meant.
    */
  private def trimReturn(line: String): String =
    if line.endsWith("\r") then line.dropRight(1) else line
