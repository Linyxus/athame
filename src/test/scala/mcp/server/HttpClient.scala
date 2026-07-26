package mcp.server

import scala.collection.mutable
import scala.concurrent.{Future, Promise}
import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport

/** A minimal HTTP client for driving the real server in tests.
  *
  * `node:http` rather than `fetch`, for one reason: an SSE stream has to be read as it arrives and
  * then abandoned mid-flight, which is three lines with a request object and considerably more
  * with a fetch body reader. The facades here are test-only and deliberately separate from the
  * server's own.
  */
@js.native
@JSImport("node:http", JSImport.Namespace)
private[server] object ClientHttp extends js.Object:
  def request(
    options: js.Dictionary[js.Any],
    callback: js.Function1[ClientResponse, Unit]
  ): ClientRequest = js.native

@js.native
private[server] trait ClientRequest extends js.Object:
  def write(chunk: String): Boolean = js.native
  def end(): Unit = js.native
  def on(event: String, listener: js.Function1[js.Any, Unit]): Unit = js.native

  /** Hangs up without waiting, which is how a client abandons an SSE stream. */
  def destroy(): Unit = js.native

@js.native
private[server] trait ClientResponse extends js.Object:
  def statusCode: Int = js.native
  def headers: js.Dictionary[js.Any] = js.native
  def setEncoding(encoding: String): Unit = js.native
  def on(event: String, listener: js.Function1[js.Any, Unit]): Unit = js.native

final case class Reply(status: Int, headers: Map[String, String], body: String):
  def sessionId: Option[String] = headers.get("mcp-session-id")
  def contentType: Option[String] = headers.get("content-type")

/** An open SSE stream. Completing the future means the response headers arrived, which is also
  * true of the error statuses, so the same call covers a refused GET.
  */
final class EventStream(handle: ClientRequest, val status: Int, val headers: Map[String, String]):
  def close(): Unit = handle.destroy()

/** Collects SSE events and lets a test await the nth one without sleeping. */
final class Events:
  private val seen = mutable.ArrayBuffer.empty[String]
  private var waiting: Option[(Int, Promise[Vector[String]])] = None

  val record: String => Unit = text =>
    seen.append(text)
    waiting match
      case Some((wanted, promise)) if seen.length >= wanted =>
        waiting = None
        promise.success(seen.toVector)
      case _ => ()

  /** Completes once at least `count` events have arrived, immediately if they already have. */
  def awaiting(count: Int): Future[Vector[String]] =
    if seen.length >= count then Future.successful(seen.toVector)
    else
      val promise = Promise[Vector[String]]()
      waiting = Some((count, promise))
      promise.future

  def all: Vector[String] = seen.toVector

final class HttpClient(port: Int, path: String = "/mcp"):

  def post(body: String, headers: Map[String, String] = Map.empty): Future[Reply] =
    send("POST", Some(body), headers)

  def delete(headers: Map[String, String] = Map.empty): Future[Reply] =
    send("DELETE", None, headers)

  def send(
    method: String,
    body: Option[String],
    headers: Map[String, String],
    at: String = path
  ): Future[Reply] =
    val promise = Promise[Reply]()
    val handle = ClientHttp.request(
      options(method, headers, at),
      response =>
        response.setEncoding("utf8")
        val buffer = StringBuilder()
        response.on("data", chunk => buffer.append(chunk.asInstanceOf[String]))
        response.on(
          "end",
          _ => promise.success(Reply(response.statusCode, headerMap(response), buffer.toString))
        )
    )
    handle.on("error", error => promise.failure(js.JavaScriptException(error)))
    body.foreach(text => handle.write(text))
    handle.end()
    promise.future

  /** Opens a GET and hands each `data:` payload to `onEvent` as it arrives. */
  def stream(headers: Map[String, String])(onEvent: String => Unit): Future[EventStream] =
    val promise = Promise[EventStream]()
    var buffer = ""
    var handle: ClientRequest = null
    handle = ClientHttp.request(
      options("GET", headers, path),
      response =>
        response.setEncoding("utf8")
        promise.success(EventStream(handle, response.statusCode, headerMap(response)))
        response.on(
          "data",
          chunk =>
            buffer += chunk.asInstanceOf[String]
            var boundary = buffer.indexOf("\n\n")
            while boundary >= 0 do
              val block = buffer.substring(0, boundary)
              buffer = buffer.substring(boundary + 2)
              if block.startsWith("data: ") then onEvent(block.substring("data: ".length))
              boundary = buffer.indexOf("\n\n")
        )
    )
    handle.on("error", error => if !promise.isCompleted then promise.failure(js.JavaScriptException(error)))
    handle.end()
    promise.future

  private def options(
    method: String,
    headers: Map[String, String],
    at: String
  ): js.Dictionary[js.Any] =
    val head = js.Dictionary[String]()
    headers.foreach((name, value) => head(name) = value)
    js.Dictionary[js.Any](
      "hostname" -> "127.0.0.1",
      "port" -> port,
      "path" -> at,
      "method" -> method,
      "headers" -> head
    )

  private def headerMap(response: ClientResponse): Map[String, String] =
    val raw = response.headers
    js.Object
      .keys(raw.asInstanceOf[js.Object])
      .toVector
      .flatMap: name =>
        raw.get(name).filter(value => value != null && !js.isUndefined(value)).map(value => name -> value.toString)
      .toMap
