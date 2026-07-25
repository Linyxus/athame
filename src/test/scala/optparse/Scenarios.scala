package optparse

/** Shared CLI definitions used by the interpreter suite (and, in phase 2, by the macro suites).
  *
  * Every `val` here is deliberately written WITHOUT a type ascription. That is not a style choice:
  * the inferred shape type is what the macro reads, and a widening ascription such as
  * `val c: Cli[?, Int] = ...` would destroy it. These vals therefore double as a demonstration that
  * plain `val` abstraction preserves the shape.
  *
  * Everything here is structurally valid. Trees that violate R1-R5 belong in the test suites, built
  * inline, so that phase 2 can safely call `compile` on every scenario in this file.
  */
object Scenarios:

  // -------------------------------------------------------------------------------------------
  // Flags only
  // -------------------------------------------------------------------------------------------

  val flagsOnly =
    flag("verbose", 'v', "Print more output") ~
      flag("quiet", 'q', "Print less output") ~
      flag("force", "Overwrite without asking")

  // -------------------------------------------------------------------------------------------
  // Typed options, one per Read instance
  // -------------------------------------------------------------------------------------------

  val typedOpts =
    opt[Int]("jobs", 'j', "Number of parallel jobs") ~
      opt[Double]("ratio", 'r', "Compression ratio") ~
      opt[Boolean]("cache", 'c', "Whether to use the cache") ~
      opt[Long]("seed", "Random seed") ~
      opt[String]("name", 'n', "Build name")

  // -------------------------------------------------------------------------------------------
  // Defaults and optional
  // -------------------------------------------------------------------------------------------

  val defaultsAndOptional =
    opt[Int]("jobs", 'j', "Number of parallel jobs").withDefault(1) ~
      opt[String]("out", 'o', "Output directory").optional ~
      flag("verbose", 'v', "Print more output")

  /** A `map` applied *above* a `Default`, which the interpreter must apply once to the final value
    * rather than per occurrence.
    */
  val mappedOverDefault =
    opt[Int]("jobs", 'j', "Number of parallel jobs").withDefault(1).map(_ * 10)

  /** A `map` applied *under* a `Default`, which must be applied per occurrence (and skipped
    * entirely when the default is used).
    */
  val mappedUnderDefault =
    opt[Int]("jobs", 'j', "Number of parallel jobs").map(_ * 10).withDefault(-1)

  // -------------------------------------------------------------------------------------------
  // Repeated options
  // -------------------------------------------------------------------------------------------

  val repeatedOpt =
    opt[String]("include", 'I', "Add a directory to the include path").repeated ~
      flag("strict", 's', "Fail on warnings")

  /** `map` under `Repeated` applies per occurrence, before collection. */
  val mappedUnderRepeated =
    opt[Int]("n", 'n', "A number").map(_ + 1).repeated

  // -------------------------------------------------------------------------------------------
  // Positionals
  // -------------------------------------------------------------------------------------------

  val twoRequiredPositionals =
    arg[String]("source", "File to copy from") ~
      arg[String]("dest", "File to copy to")

  val positionalWithDefault =
    arg[String]("source", "File to read") ~
      arg[Int]("count", "How many lines").withDefault(10)

  val positionalRepeatedLast =
    arg[String]("command", "Command to run") ~
      arg[String]("rest", "Arguments passed through").repeated

  /** Options and positionals in the same scope, so tests can interleave them. */
  val optionsAndPositionals =
    flag("verbose", 'v', "Print more output") ~
      opt[Int]("jobs", 'j', "Number of parallel jobs").withDefault(1) ~
      arg[String]("target", "Build target")

  /** Exercises `-`, `-2` and negative option values as positional-looking tokens. */
  val numericPositionals =
    opt[Int]("offset", 'o', "Offset into the stream").withDefault(0) ~
      arg[String]("path", "Input path, or `-` for stdin") ~
      arg[Int]("count", "How many items").withDefault(1)

  // -------------------------------------------------------------------------------------------
  // pure
  // -------------------------------------------------------------------------------------------

  val withPure =
    pure(42) ~ flag("on", "Turn it on")

  // -------------------------------------------------------------------------------------------
  // map into a case class
  // -------------------------------------------------------------------------------------------

  case class BuildConfig(target: String, jobs: Int, verbose: Boolean)

  val mappedCaseClass =
    (arg[String]("target", "Build target") ~
      opt[Int]("jobs", 'j', "Number of parallel jobs").withDefault(1) ~
      flag("verbose", 'v', "Print more output"))
      .map { case ((target, jobs), verbose) => BuildConfig(target, jobs, verbose) }

  // -------------------------------------------------------------------------------------------
  // A git-like CLI: global flag, two levels of subcommands, sealed result hierarchy
  // -------------------------------------------------------------------------------------------

  sealed trait GitCommand
  case class Clone(repo: String, depth: Option[Int], quiet: Boolean) extends GitCommand
  case class RemoteAdd(name: String, url: String) extends GitCommand
  case class RemoteRemove(name: String) extends GitCommand
  case object RemoteList extends GitCommand
  case class Git(verbose: Boolean, command: GitCommand)

  val gitClone =
    sub(
      "clone",
      "Clone a repository into a new directory",
      (arg[String]("repo", "Repository to clone") ~
        opt[Int]("depth", "Create a shallow clone of that depth").optional ~
        flag("quiet", 'q', "Suppress progress output"))
        .map { case ((repo, depth), quiet) => Clone(repo, depth, quiet) }
    )

  val gitRemote =
    sub(
      "remote",
      "Manage the set of tracked repositories",
      sub(
        "add",
        "Add a remote named <name>",
        (arg[String]("name", "Name of the remote") ~ arg[String]("url", "URL of the remote"))
          .map((name, url) => RemoteAdd(name, url))
      ) | sub(
        "remove",
        "Remove the remote named <name>",
        arg[String]("name", "Name of the remote").map(RemoteRemove(_))
      ) | sub(
        "list",
        "List every remote",
        pure(RemoteList)
      )
    )

  val git =
    (flag("verbose", 'v', "Print more output") ~ (gitClone | gitRemote))
      .map((verbose, command) => Git(verbose, command))

  // -------------------------------------------------------------------------------------------
  // A docker-like CLI: repeated options plus repeated trailing positionals
  // -------------------------------------------------------------------------------------------

  case class DockerRun(
    image: String,
    command: List[String],
    env: List[String],
    detach: Boolean,
    name: Option[String]
  )

  val dockerRun =
    (opt[String]("env", 'e', "Set an environment variable").repeated ~
      flag("detach", 'd', "Run the container in the background") ~
      opt[String]("name", "Assign a name to the container").optional ~
      arg[String]("image", "Image to run") ~
      arg[String]("command", "Command and arguments for the container").repeated)
      .map { case ((((env, detach), name), image), command) =>
        DockerRun(image, command, env, detach, name)
      }

  val docker =
    sub("run", "Create and run a new container from an image", dockerRun)

  // -------------------------------------------------------------------------------------------
  // A subcommand group whose branches have unrelated result types, yielding a union
  // -------------------------------------------------------------------------------------------

  val unionResult =
    sub("count", "Yield a number", opt[Int]("value", 'n', "The number")) |
      sub("label", "Yield a string", arg[String]("text", "The text"))
