package com.tinfoiled.docopt4s

/** A base class for a Docopt oriented command line application that can take multiple subcommands (tasks). */
trait MultiTaskMain {

  /** The application name. */
  val Name: String

  /** The version to print when the --version flag is set. */
  val Version: String

  /** A list of subcommands that can be executed. */
  val Tasks: Seq[Task]

  /** The full, main docopt that can be used for the application. */
  lazy val Doc: String = SimpleDoc

  /** The simple docopt without any description or additional text. */
  val SimpleDoc: String = UsageDoc + "\n\n" + OptionsDoc + "\n\n" + CommandsDoc

  /** A suggested usage section for the basic docopt. */
  lazy val UsageDoc: String =
    s"""Usage:
       |  $Name [--debug] <command> [args...]""".stripMargin

  /** A suggested options section for the docopt. */
  lazy val OptionsDoc: String =
    s"""Options:
       |  -h --help  Show this screen.
       |  --version  Show version.
       |  --debug    Log extra information to the console while executing.""".stripMargin

  /** A suggested commands section for the docopt. */
  lazy val CommandsDoc: String =
    """Commands:
      |%s""".stripMargin.format {
      // Align the task subcommands and descriptions to the longest subcommand.
      val col = (0 +: Tasks.map(_.Cmd.length)).max
      Tasks
        .map(task => s"%${col + 2}s  %s".format(task.Cmd, task.Description))
        .mkString("\n")
    }.trim

  /** Runs the tool. This does not handle any docopt exception automatically while parsing the command line.
    *
    * @param args
    *   command-line arguments as described in [[Doc]]
    */
  @throws[DocoptException]
  def go(args: String*): Unit = {

    // Java docopt doesn't support ignoring options after the command, so strip them first.
    val mainArgs: Seq[String] = if (args.nonEmpty) {
      val (options, cmd) = args.span(_.startsWith("-"))
      if (cmd.isEmpty) options :+ "???" else options :+ cmd.head
    } else Seq("--help")

    // Get the command, but throw exceptions for --help and --version
    val cmd = Docopt(Doc, Version, mainArgs, optionsFirst = true).string.getOption("<command>") match {
      case Some(cmd) if "???" != cmd  => cmd
      case _ => throw new DocoptException("Missing command", exitCode = 1, docopt = Doc)
    }

    // Reparse with the specific command.
    val task = Tasks
      .find(_.Cmd == cmd)
      .getOrElse(throw new DocoptException(s"Unknown command: $cmd", exitCode = 1, docopt = Doc))

    try {
      task.go(Docopt(task.Doc, Version, args))
    } catch {
      // This is only here to rewrap any internal docopt exception with the current docopt
      case ex: DocoptException =>
        throw new DocoptException(ex.getMessage, ex, task.Doc, ex.exitCode)
      case ex: org.docopt.DocoptExitException =>
        throw new DocoptException(ex.getMessage, ex, task.Doc, ex.getExitCode)
    }
  }

  def main(args: Array[String]): Unit = {
    // All the command is executed in the go method, and this wraps DocOpt and exceptions for console feedback.
    try {
      go(args.toIndexedSeq: _*)
    } catch {
      case ex: org.docopt.DocoptExitException =>
        Option(if (ex.getExitCode == 0) System.out else System.err)
          .foreach(ps => {
            if (ex.getMessage != null) ps.println(ex.getMessage)
            else ps.println(Doc)
          })
        System.exit(ex.getExitCode)
      case ex: DocoptException =>
        println(ex.docopt)
        if (ex.getMessage != null) {
          println()
          println(ex.getMessage)
        }
        System.exit(1)
      case ex: Exception =>
        println(Doc)
        println()
        ex.printStackTrace()
        System.exit(1)
    }
  }
}
