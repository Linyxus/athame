package sync

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport

/** The little of `node:fs`/`os`/`path` these tests need. The git package's test facades are
  * `private[git]`, so this package brings its own rather than widening theirs.
  */
@js.native
@JSImport("node:fs", JSImport.Namespace)
private[sync] object TestFs extends js.Object:
  def mkdtempSync(prefix: String): String = js.native
  def rmSync(path: String, options: js.Any): Unit = js.native

@js.native
@JSImport("node:os", JSImport.Namespace)
private[sync] object TestOs extends js.Object:
  def tmpdir(): String = js.native

@js.native
@JSImport("node:path", JSImport.Namespace)
private[sync] object TestPath extends js.Object:
  def join(parts: String*): String = js.native

@js.native
@JSImport("node:process", JSImport.Default)
private[sync] object TestProcess extends js.Object:
  def env: js.Dictionary[String] = js.native

/** Gives a suite its own temp directory for the duration of its run.
  *
  * Snapshots put their temp index in the OS temp directory, and sbt runs each suite in a separate
  * Node process, concurrently — so a suite that creates snapshots drops files into a directory
  * another suite may be watching. `TMPDIR` is read by `os.tmpdir()` on every call, so redirecting it
  * moves this process's temp files, and only this process's, somewhere nobody else is looking.
  */
object PrivateTmpdir:
  /** Claims a private temp directory; the returned thunk restores the old one and removes it. */
  def claim(): () => Unit =
    val previous = TestProcess.env.get("TMPDIR")
    val own = TestFs.mkdtempSync(TestPath.join(TestOs.tmpdir(), "ame-suite-"))
    TestProcess.env("TMPDIR") = own
    () =>
      previous match
        case Some(value) => TestProcess.env("TMPDIR") = value
        case None        => TestProcess.env -= "TMPDIR"
      val options = js.Dictionary.empty[js.Any]
      options("recursive") = true
      options("force") = true
      TestFs.rmSync(own, options)

/** A directory that exists but holds no repository — the fixture for "this is not a repo". */
object TempDir:
  def plain(body: String => Unit): Unit =
    val dir = TestFs.mkdtempSync(TestPath.join(TestOs.tmpdir(), "ame-plain-"))
    val options = js.Dictionary.empty[js.Any]
    options("recursive") = true
    options("force") = true
    try body(dir)
    finally TestFs.rmSync(dir, options)
