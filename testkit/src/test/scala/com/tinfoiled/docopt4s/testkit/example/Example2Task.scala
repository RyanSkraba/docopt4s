package com.tinfoiled.docopt4s.testkit.example

import com.tinfoiled.docopt4s.DocoptCliGo

/** An example task that can be integrated into the main [[ExampleGo]] driver. This implements the standard Naval Fate
  * docopt example.
  */
object Example2Task extends DocoptCliGo.Task {

  val Cmd = "naval_fate"

  val Description = "The naval fate example for docopts."

  val Doc: String =
    s"""$Description
       |
       |Usage:
       |  ${ExampleGo.Cli} $Cmd ship new <name>...
       |  ${ExampleGo.Cli} $Cmd ship <name> move <x> <y> [--speed=<kn>]
       |  ${ExampleGo.Cli} $Cmd ship shoot <x> <y>
       |  ${ExampleGo.Cli} $Cmd mine (set|remove) <x> <y> [--moored|--drifting]
       |  ${ExampleGo.Cli} $Cmd -h | --help
       |  ${ExampleGo.Cli} $Cmd --version
       |
       |Options:
       |  -h --help     Show this screen.
       |  --version     Show version.
       |  --speed=<kn>  Speed in knots [default: 10].
       |  --moored      Moored (anchored) mine.
       |  --drifting    Drifting mine.
       |
       |""".stripMargin.trim

  def go(opts: TaskOptions): Unit = {
    val file: String = opts.getString("FILE")
    val tableArg: String = opts.getString("TABLE")
    val ignore: Boolean = opts.getBoolean("--ignore")
  }
}
