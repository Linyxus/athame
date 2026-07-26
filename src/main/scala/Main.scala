import optparse.*

/** Entry point: parse, run, print, exit.
  *
  * All three of those steps live elsewhere — the grammar in [[ame.Cli]], the work in
  * [[ame.Runner]] — so that everything except the process itself can be tested without spawning a
  * binary. This is the only place in the main sources that calls `compile`.
  */
@main def run(): Unit =
  import scala.scalajs.js
  val process = js.Dynamic.global.process
  val argv = process.argv.asInstanceOf[js.Array[String]].toList.drop(2)
  val parser = compile(ame.Cli.cli)
  parser.parse(argv) match
    case Right(command) =>
      // The runner already newline-terminates its lines, so write them through rather than
      // println, which would add a second one.
      val outcome = ame.Runner.execute(command, process.cwd().asInstanceOf[String])
      process.stdout.write(outcome.out)
      process.stderr.write(outcome.err)
      process.exitCode = outcome.exit
    // `--help` is a successful run that happens to arrive as a Left: print the scope's own help
    // and leave the exit code at 0.
    case Left(ParseError.HelpRequested(text)) => println(text)
    case Left(error) =>
      println(s"error: $error")
      println(parser.help)
      process.exitCode = 1
