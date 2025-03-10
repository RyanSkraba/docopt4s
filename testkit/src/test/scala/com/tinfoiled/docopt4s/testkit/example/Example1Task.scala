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
       |  ${ExampleGo.Name} $Cmd ARG1 [ARG2] [ARG3...] [--flag]
       |
       |Options:
       |  -h --help  Show this screen.
       |  --version  Show version.
       |  ARG1       Mandatory argument
       |  ARG2       Optional argument [Default: 0]
       |  ARG3       Repeated argument
       |  --flag     A flag argument
       |
       |Some description of the command line interface task
       |""".stripMargin.trim

  def go(opts: TaskOptions): Unit = {
    println(s"Command: $Cmd")
    println(s"ARG1: ${opts.getString("ARG1")}")
    println(s"ARG2: ${opts.getStringOption("ARG2")}")
    println(s"ARG3: ${opts.getStrings("ARG3")}")
    println(s"--flag: ${opts.getBoolean("--flag")}")
  }
}
