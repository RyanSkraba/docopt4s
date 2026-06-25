package com.tinfoiled.docopt4s.testkit.example

import com.tinfoiled.docopt4s.{AnsiConsole, Docopt, Task}

/** A echo task reads from stdin and echos each line. */
object EchoTask extends Task {

  override val Cmd = "echo"

  val Description = "Echo each line."

  val Doc: String =
    s"""$Description
       |
       |Usage:
       |  ${ExampleGo.Name} $Cmd
       |
       |Options:
       |  -h --help    Show this screen.
       |  --version    Show version.
       |""".stripMargin.trim

  def go(opt: Docopt): Unit = {

    val out: AnsiConsole = AnsiConsole(verbose = true)

    val reader = LazyList
      .from(0)
      .map(i => {
        (i, Option(Console.in.readLine())) match {
          case (0, found @ Some(_)) =>
            out.vPrintln("Reading from stdin (Ctrl+D to finish):")
            found
          case (_, found @ Some(_)) =>
            found
          case (n, None) =>
            out.vPrintln(s"Finished reading from stdin after $n lines")
            None
        }
      })
      .takeWhile(_.nonEmpty)
      .flatten
      .iterator

    for (line <- reader)
      out.vPrintln(line)
  }
}
