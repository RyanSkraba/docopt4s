package com.tinfoiled.docopt4s.testkit.example

import com.tinfoiled.docopt4s.{Docopt, Task}

/** An example task that can be integrated into the main [[ExampleGo]] driver. */
object Example1Task extends Task {

  val Cmd = "example1"

  val Description = "The first example task for the utility."

  val Doc: String =
    s"""$Description
       |
       |Usage:
       |  ${ExampleGo.Name} $Cmd [--default DEFAULT|--options] ARG1 [ARG2] [ARG3] [ARG4...]
       |
       |Options:
       |  -h --help          Show this screen.
       |  --version          Show version.
       |  --default=DEFAULT  If present, use as the default value for string arguments.
       |  --options          If present, get the arguments as options.
       |  ARG1               Mandatory argument.
       |  ARG2               Optional integer argument.
       |  ARG3               Optional argument.
       |  ARG4               Repeated argument.
       |
       |Some description of the command line interface task
       |""".stripMargin.trim

  def go(opt: Docopt): Unit = {
    println(s"Command:$Cmd")
    if (opt.getBoolean("--options")) {
      print(s"""--options:${opt.getBooleanOption("--options")}
           |--default:${opt.string.getOption("--default")}
           |ARG1:${opt.string.getOption("ARG1")}
           |ARG2:${opt.getIntOption("ARG2")}
           |ARG3:${opt.string.getOption("ARG3")}
           |ARG4:${opt.strings.getOption("ARG4").map(_.mkString("(", ",", ")"))}
           |""".stripMargin)
    } else if (opt.string.getOption("--default").nonEmpty) {
      val dflt = opt.string.get("--default")
      print(s"""--options:${opt.getBoolean("--options", default = false)}
           |--default:$dflt
           |ARG1:${opt.string.get("ARG1", dflt)}
           |ARG2:${opt.getInt("ARG2", -1)}
           |ARG3:${opt.string.get("ARG3", dflt)}
           |ARG4:${opt.strings.get("ARG4", Seq(dflt)).mkString("(", ",", ")")}
           |""".stripMargin)
    } else {
      print(s"""--options:${opt.getBoolean("--options")}
           |--default:${opt.string.getOption("--default")}
           |ARG1:${opt.string.get("ARG1")}
           |ARG2:${opt.getInt("ARG2", default = 0)}
           |ARG3:${opt.string.get("ARG3", default = "--")}
           |ARG4:${opt.strings.get("ARG4").mkString("(", ",", ")")}
           |""".stripMargin)
    }
  }
}
