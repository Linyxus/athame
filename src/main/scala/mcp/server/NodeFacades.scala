package mcp.server

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport

/** The Node surface this package touches, and nothing more.
  *
  * Facades rather than shims: each `@JSImport` becomes a static ESM import in the linker output,
  * which is why nothing here reaches for `require`. Everything Node-shaped is confined to this
  * file so that the transports above it stay readable and the logic below it stays testable
  * without a process.
  */
@js.native
private[server] trait ReadableStream extends js.Object:
  /** Hands `data` events to their listener as `String`.
    *
    * Load-bearing for stdio: Node's decoder holds a multibyte character that straddles a chunk
    * boundary until the rest of it arrives, so [[LineFrames]] never sees half a character.
    */
  def setEncoding(encoding: String): Unit = js.native

  def on(event: String, listener: js.Function1[js.Any, Unit]): Unit = js.native

@js.native
private[server] trait WritableStream extends js.Object:
  def write(chunk: String): Boolean = js.native

@js.native
@JSImport("node:process", JSImport.Default)
private[server] object NodeProcess extends js.Object:
  def stdin: ReadableStream = js.native
  def stdout: WritableStream = js.native

/** An inbound HTTP request. Node lower-cases every header name before it reaches us, which is why
  * lookups here are all lower-case and no case folding happens anywhere else.
  */
@js.native
private[server] trait IncomingMessage extends js.Object:
  def method: String = js.native

  /** Path and query together, as sent. */
  def url: String = js.native

  def headers: js.Dictionary[js.Any] = js.native
  def setEncoding(encoding: String): Unit = js.native
  def on(event: String, listener: js.Function1[js.Any, Unit]): Unit = js.native

@js.native
private[server] trait ServerResponse extends js.Object:
  def writeHead(status: Int, headers: js.Dictionary[String]): Unit = js.native
  def write(chunk: String): Boolean = js.native
  def end(): Unit = js.native
  def end(chunk: String): Unit = js.native

  /** `close` here means the connection went away.
    *
    * Not to be confused with `IncomingMessage`'s `close`, which fires as soon as the *request* is
    * complete — for a GET with no body, almost immediately, and long before the client has stopped
    * listening. Watching the wrong one detaches a live SSE stream the instant it opens.
    */
  def on(event: String, listener: js.Function1[js.Any, Unit]): Unit = js.native

  /** Puts the status line and headers on the wire now.
    *
    * `writeHead` only records them; Node sends them with the first `write` or `end`. For an
    * ordinary response that is invisible, but an event stream may have nothing to say for a while,
    * and until its headers arrive the client does not know the stream opened at all — it just
    * waits.
    */
  def flushHeaders(): Unit = js.native

@js.native
private[server] trait HttpListener extends js.Object:
  def listen(port: Int, host: String, callback: js.Function0[Unit]): Unit = js.native

  /** Waits for open connections to drain, so any SSE stream must be ended first. */
  def close(callback: js.Function0[Unit]): Unit = js.native

  /** `{ address, family, port }` once listening — the only way to learn an ephemeral port. */
  def address(): js.Dynamic = js.native

@js.native
@JSImport("node:http", JSImport.Namespace)
private[server] object NodeHttp extends js.Object:
  def createServer(
    handler: js.Function2[IncomingMessage, ServerResponse, Unit]
  ): HttpListener = js.native

@js.native
@JSImport("node:crypto", JSImport.Namespace)
private[server] object NodeCrypto extends js.Object:
  def randomUUID(): String = js.native
