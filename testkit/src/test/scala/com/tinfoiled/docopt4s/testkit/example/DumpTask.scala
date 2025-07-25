package com.tinfoiled.docopt4s.testkit.example

import com.tinfoiled.docopt4s.{Docopt, Task}

import scala.util.Try

/** An example task that only writes information about all the arguments it has parsed. */
object DumpTask extends Task {

  val Cmd = "dump"

  val Description = "The first example task for the utility."

  val Doc: String =
    s"""$Description
       |
       |Usage:
       |  ${ExampleGo.Name} $Cmd string [--default DEFAULT|--options] ARG1 [ARG2] [ARG3...]
       |
       |Options:
       |  -h --help          Show this screen.
       |  --version          Show version.
       |  --default=DEFAULT  If present, use as the default value for string arguments.
       |  --options          If present, get the arguments as options.
       |  ARG1               Mandatory argument.
       |  ARG2               Optional argument.
       |  ARG3               Repeated argument.
       |
       |Some description of the command line interface task
       |""".stripMargin.trim

  def go(opt: Docopt): Unit = {
    if (opt.flag("string")) {
      print("string ")
      opt.string.getOption("--default").map { dflt =>
        val args = Seq(
          opt.string.getOr("ARG1", dflt),
          opt.string.getOr("ARG2", dflt),
          opt.strings.getOr("ARG3", Seq(dflt)).mkString("<", ",", ">")
        )
        println(s"ARGS:" + args.mkString(":"))
      } getOrElse {
        if (opt.flag("--options")) {
          val args = Seq(
            opt.string.getOption("ARG1"),
            opt.string.getOption("ARG2"),
            opt.strings.getOption("ARG3").map(_.mkString("<", ",", ">"))
          )
          println(s"ARGS:" + args.mkString(":"))
        } else {
          val args = Seq(
            opt.string.get("ARG1"),
            Try(opt.string.get("ARG2")).getOrElse("--"),
            opt.strings.get("ARG3").mkString("<", ",", ">")
          )
          println(s"ARGS:" + args.mkString(":"))
        }
      }
    }
  }
}
