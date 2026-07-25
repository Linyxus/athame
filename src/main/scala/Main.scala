import optparse.*

object Ame:
  val version = "0.0.1-SNAPSHOT"

  val cli = sub("version", "Print the version number", pure(version), help = false)

@main def run(): Unit =
  import scala.scalajs.js
  val argv = js.Dynamic.global.process.argv.asInstanceOf[js.Array[String]].toList.drop(2)
  val parser = compile(Ame.cli)
  parser.parse(argv) match
    case Right(output) => println(output)
    // `--help` is a successful run that happens to arrive as a Left: print the scope's own help
    // and leave the exit code at 0.
    case Left(ParseError.HelpRequested(text)) => println(text)
    case Left(error) =>
      println(s"error: $error")
      println(parser.help)
      js.Dynamic.global.process.exitCode = 1
