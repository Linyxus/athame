import optparse.*

object Ame:
  sealed trait Command
  case class Build(target: String, jobs: Int, verbose: Boolean) extends Command
  case class Clean(dryRun: Boolean) extends Command

  val cli =
    sub(
      "build",
      "Compile a target",
      (arg[String]("target", "Target to build") ~
        opt[Int]("jobs", 'j', "Parallel jobs").withDefault(1) ~
        flag("verbose", 'v', "Chatty output"))
        .map { case ((target, jobs), verbose) => Build(target, jobs, verbose) }
    ) | sub(
      "clean",
      "Remove build outputs",
      flag("dry-run", 'n', "Only print what would be removed").map(Clean.apply)
    )

@main def run(): Unit =
  import scala.scalajs.js
  val argv = js.Dynamic.global.process.argv.asInstanceOf[js.Array[String]].toList.drop(2)
  val parser = compile(Ame.cli)
  parser.parse(argv) match
    case Right(command) => println(s"parsed: $command")
    case Left(error) =>
      println(s"error: $error")
      println(parser.help)
      js.Dynamic.global.process.exitCode = 1
