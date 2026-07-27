import scala.sys.process.Process

ThisBuild / scalaVersion := "3.8.4"

lazy val packageBundle = taskKey[File]("Bundle the Scala.js output into one CommonJS file at target/sea/bundle.cjs")
lazy val packageBinary = taskKey[File]("Package the application as a Node.js Single Executable Application in dist/ame")
lazy val packagePlugin = taskKey[File]("Refresh the magic plugin's committed bundle at plugin/bin/ame.cjs")

def runCommand(cmd: Seq[String], cwd: File, log: sbt.Logger): Unit = {
  log.info(cmd.mkString(" "))
  val exit = Process(cmd, cwd).!
  if (exit != 0) sys.error(s"Command failed with exit code $exit: ${cmd.mkString(" ")}")
}

lazy val root = project
  .in(file("."))
  .enablePlugins(ScalaJSPlugin)
  .settings(
    name := "athame",
    scalaJSUseMainModuleInitializer := true,
    scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.ESModule) },
    libraryDependencies += "org.scalameta" %%% "munit" % "1.1.1" % Test,
    // The one bundling step, shared: the SEA binary and the plugin ship the same bytes, so a
    // behavior change reaches both or neither.
    packageBundle := {
      val log = streams.value.log
      val report = (Compile / fullLinkJS).value.data
      val linkerOutput = (Compile / fullLinkJS / scalaJSLinkerOutputDirectory).value
      val mainModule = report.publicModules.headOption.getOrElse(sys.error("No public module produced by fullLinkJS"))
      val mainJs = linkerOutput / mainModule.jsFileName

      val workDir = target.value / "sea"
      IO.createDirectory(workDir)

      // SEA main scripts must be CommonJS; bundle the ESM linker output into one CJS file.
      val bundle = workDir / "bundle.cjs"
      runCommand(
        Seq(
          "npx", "--yes", "esbuild", mainJs.getAbsolutePath,
          "--bundle", "--platform=node", "--format=cjs",
          s"--outfile=${bundle.getAbsolutePath}"
        ),
        workDir, log
      )
      bundle
    },
    packageBinary := {
      val log = streams.value.log
      val bundle = packageBundle.value
      val workDir = bundle.getParentFile
      val distDir = baseDirectory.value / "dist"
      IO.createDirectory(distDir)
      val binary = distDir / "ame"

      IO.write(
        workDir / "sea-config.json",
        """{
          |  "main": "bundle.cjs",
          |  "output": "sea-prep.blob",
          |  "disableExperimentalSEAWarning": true
          |}""".stripMargin
      )
      runCommand(Seq("node", "--experimental-sea-config", "sea-config.json"), workDir, log)

      val nodeExe = file(Process(Seq("node", "-e", "console.log(process.execPath)")).!!.trim)
      IO.copyFile(nodeExe, binary)
      binary.setExecutable(true)

      val isMac = System.getProperty("os.name").toLowerCase.contains("mac")
      if (isMac) runCommand(Seq("codesign", "--remove-signature", binary.getAbsolutePath), workDir, log)

      runCommand(
        Seq(
          "npx", "--yes", "postject", binary.getAbsolutePath,
          "NODE_SEA_BLOB", "sea-prep.blob",
          "--sentinel-fuse", "NODE_SEA_FUSE_fce680ab2cc467b6e072b8b5df1996b2"
        ) ++ (if (isMac) Seq("--macho-segment-name", "NODE_SEA") else Seq.empty),
        workDir, log
      )

      if (isMac) runCommand(Seq("codesign", "--sign", "-", binary.getAbsolutePath), workDir, log)

      log.info(s"SEA binary written to $binary")
      binary
    },
    // The plugin runs the bundle under whatever `node` is on the user's PATH, so it needs the
    // CommonJS file itself rather than the SEA binary, which is built for this machine only.
    packagePlugin := {
      val log = streams.value.log
      val bundle = packageBundle.value
      val shipped = baseDirectory.value / "plugin" / "bin" / "ame.cjs"
      IO.createDirectory(shipped.getParentFile)
      IO.copyFile(bundle, shipped)
      log.info(s"plugin bundle written to $shipped")
      shipped
    },
  )
