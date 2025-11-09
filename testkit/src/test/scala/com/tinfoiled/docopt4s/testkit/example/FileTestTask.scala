package com.tinfoiled.docopt4s.testkit.example

import com.tinfoiled.docopt4s.{Docopt, PathValidator, Task}

import scala.reflect.io.Path

/** A file test just verifies whether an argument is a path, file, directory and/or exists. */
object FileTestTask extends Task {

  val Cmd = "filetest"

  val Description = "Run a test case."

  val Doc: String =
    s"""$Description
       |
       |Usage:
       |  ${ExampleGo.Name} $Cmd [(--file|--dir)] [(--exists|--no-exists)]
       |      [--tag=TAG] PATH
       |
       |Options:
       |  -h --help    Show this screen.
       |  --version    Show version.
       |  --file       The path must be a file
       |  --dir        The path must be a directory
       |  --exists     The path must exist
       |  --no-exists  The path must not exist
       |  --tag=TAG    The tag assigned to the path (used in the error message)
       |  PATH         The path to check
       |""".stripMargin.trim

  def go(opt: Docopt): Unit = {

    val vld =
      if (opt.flag("--exists")) PathValidator().exists()
      else if (opt.flag("--no-exists")) PathValidator().doesntExist()
      else PathValidator().optionallyExists()

    val vldTag = opt.string.getOption("--tag").map(vld.withTag).getOrElse(vld)

    val validated: Path = if (opt.flag("--file")) {
      opt.file.get("PATH", vldTag)
    } else if (opt.flag("--dir")) {
      opt.dir.get("PATH", vldTag)
    } else {
      opt.path.get("PATH", vldTag)
    }

    println(s"OK $validated")
  }
}
