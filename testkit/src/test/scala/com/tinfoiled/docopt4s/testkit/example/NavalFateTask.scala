package com.tinfoiled.docopt4s.testkit.example

import com.tinfoiled.docopt4s.{Docopt, DocoptException, Task}

/** An example task that can be integrated into the main [[ExampleGo]] driver. This implements the standard Naval Fate
  * docopt example.
  */
object NavalFateTask extends Task {

  val Cmd = "naval_fate"

  val Description = "The naval fate example for docopt."

  val Doc: String =
    s"""$Description
       |
       |Usage:
       |  ${ExampleGo.Name} $Cmd ship new <name>...
       |  ${ExampleGo.Name} $Cmd ship <name> move <x> <y> [--speed=<kn>]
       |  ${ExampleGo.Name} $Cmd ship shoot <x> <y>
       |  ${ExampleGo.Name} $Cmd mine (set|remove) <x> <y> [--moored|--drifting]
       |  ${ExampleGo.Name} $Cmd -h | --help
       |  ${ExampleGo.Name} $Cmd --version
       |
       |Options:
       |  -h --help     Show this screen.
       |  --version     Show version.
       |  --speed=<kn>  Speed in knots [default: 10].
       |  --moored      Moored (anchored) mine.
       |  --drifting    Drifting mine.
       |
       |""".stripMargin.trim

  def go(opt: Docopt): Unit = {
    if (opt.boolean.get("ship") && opt.boolean.get("new")) {
      val names = opt.strings.get("<name>")
      if (names.isEmpty) throw new DocoptException("Ship name is required.")
      else for (name <- names) println(s"Creating ship: $name")
    } else if (opt.boolean.get("ship") && opt.boolean.get("move")) {
      println(s"""Moving ${opt.string.get("<name>")} ship
        |  to coordinates (${opt.int.get("<x>")}, ${opt.int.get("<y>")})
        |  at speed ${opt.int.get("--speed")}""".stripMargin)
    } else if (opt.boolean.get("ship") && opt.boolean.get("shoot")) {
      println(s"Shooting at coordinates (${opt.int.get("<x>")}, ${opt.int.get("<y>")})")
    } else if (opt.boolean.get("mine")) {
      val verb = if (opt.flag("set")) "Setting" else "Removing"
      val adjective = if (opt.flag("--moored")) "moored " else if (opt.flag("--drifting")) "drifting " else ""
      println(s"""$verb a ${adjective}mine
                 |  at coordinates (${opt.int.get("<x>")}, ${opt.int.get("<y>")})""".stripMargin)
    } else {
      throw new DocoptException("Unknown parsing error")
    }
  }
}
