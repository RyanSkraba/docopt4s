package com.tinfoiled.docopt4s.testkit.example

import com.tinfoiled.docopt4s.{Docopt, DocoptException, Task}

/** An example task that can be integrated into the main [[ExampleGo]] driver. This implements the standard Naval Fate
  * docopt example.
  */
object Example2Task extends Task {

  val Cmd = "naval_fate"

  val Description = "The naval fate example for docopts."

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

  def go(opts: Docopt): Unit = {
    if (opts.getBoolean("ship") && opts.getBoolean("new")) {
      val names = opts.getStrings("<name>")
      if (names.isEmpty) throw new DocoptException("Ship name is required.")
      else for (name <- names) println(s"Creating ship: $name")
    } else if (opts.getBoolean("ship") && opts.getBoolean("move")) {
      println(s"""Moving ${opts.getString("<name>")} ship
        |  to coordinates (${opts.getInt("<x>")}, ${opts.getInt("<y>")})
        |  at speed ${opts.getInt("--speed")}""".stripMargin)
    } else if (opts.getBoolean("ship") && opts.getBoolean("shoot")) {
      println(s"Shooting at coordinates (${opts.getInt("<x>")}, ${opts.getInt("<y>")})")
    } else {
      throw new DocoptException("Unknown parsing error")
    }
  }
}
