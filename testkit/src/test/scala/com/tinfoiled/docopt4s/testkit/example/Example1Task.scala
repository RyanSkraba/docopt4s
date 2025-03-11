package com.tinfoiled.docopt4s.testkit.example

import com.tinfoiled.docopt4s.Task

/** An example task that can be integrated into the main [[ExampleGo]] driver. */
object Example1Task extends Task {

  val Cmd = "example1"

  val Description = "The first example task for the utility."

  val Doc: String =
    s"""$Description
       |
       |Usage:
       |  ${ExampleGo.Name} $Cmd [--default DEFAULT|--options] ARG1 [ARG2] [ARG3...]
       |
       |Options:
       |  -h --help          Show this screen.
       |  --version          Show version.
       |  --default=DEFAULT  If present, use as the default value for string arguments.
       |  --options          If present, get the arguments as options.
       |  ARG1               Mandatory argument.
       |  ARG2               Optional integer argument.
       |  ARG3               Repeated argument.
       |
       |Some description of the command line interface task
       |""".stripMargin.trim

  def go(opts: TaskOptions): Unit = {
    println(s"Command:$Cmd")
    if (opts.getBoolean("--options")) {
      print(s"""--options:${opts.getBooleanOption("--options")}
           |--default:${opts.getStringOption("--default")}
           |ARG1:${opts.getStringOption("ARG1")}
           |ARG2:${opts.getIntOption("ARG2")}
           |ARG3:${opts.getStringsOption("ARG3").map(_.mkString("(", ",", ")"))}
           |""".stripMargin)
    } else if (opts.getStringOption("--default").nonEmpty) {
      val dflt = opts.getString("--default")
      print(s"""--options:${opts.getBoolean("--options")}
           |--default:$dflt}
           |ARG1:${opts.getString("ARG1", dflt)}
           |ARG2:${opts.getInt("ARG2", -1)}
           |ARG3:${opts.getStrings("ARG3", Seq(dflt)).map(_.mkString("(", ",", ")"))}
           |""".stripMargin)
    } else {
      print(s"""--options:${opts.getBoolean("--options")}
           |--default:${opts.getStringOption("--default")}
           |ARG1:${opts.getString("ARG1")}
           |ARG2:${opts.getInt("ARG2", default = 0)}
           |ARG3:${opts.getStrings("ARG3").mkString("(", ",", ")")}
           |""".stripMargin)
    }
  }
}
