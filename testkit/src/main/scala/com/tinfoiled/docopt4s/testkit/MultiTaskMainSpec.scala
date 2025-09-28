package com.tinfoiled.docopt4s.testkit

import com.tinfoiled.docopt4s.AnsiConsole.withConsoleMatch
import com.tinfoiled.docopt4s.{MultiTaskMain, Task, DocoptException}
import org.scalactic.source
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

import scala.reflect.ClassTag

/** Unit test specification base for an [[MultiTaskMain]] implementation.
  *
  * @param Main
  *   The main driver to be tested.
  * @param Task
  *   Optionally, the subtask of the main driver.
  */
abstract class MultiTaskMainSpec[Tsk <: Task](protected val Main: MultiTaskMain, protected val Task: Option[Tsk] = None)
    extends AnyFunSpecLike
    with Matchers {

  /** The Docopt for the Task (if present) but defaulting to the Cli if not. */
  lazy val Doc: String = Task.map(_.Doc).getOrElse(Main.Doc)

  /** Shortcut to the main script name for descriptions. */
  lazy val MainName: String = Main.Name

  /** The command used to specify the task (if present) or the empty string if not. */
  lazy val TaskCmd: String = Task.map(_.Cmd).getOrElse("")

  /** Either the task command as a Seq, or empty. Used for building arguments. */
  lazy val TaskCmdArg: Seq[String] = Task.map(_.Cmd).toSeq

  /** The command used to specify the task with a space suffix (if present) or the empty string if not. */
  lazy val TaskCmdPost: String = Task.map(_.Cmd + " ").getOrElse("")

  /** The command used to specify the task with a space prefix (if present) or the empty string if not. */
  lazy val TaskCmdPre: String = Task.map(_.Cmd + " ").getOrElse("")

  /** A string that is guaranteed not to exist in the Doc. */
  lazy val UnknownTxt: String = ("garbage" +: LazyList.from(0).map("garbage" + _)).dropWhile(Doc.contains).head

  describe(Main.Name + s"$TaskCmdPre docopt check") {
    it(s"should have less than 80 characters per string for readability.") {
      for (line <- Doc.split("\n")) {
        withClue(line) {
          line.length should be < 80
        }
      }
    }
    if (Task.isEmpty) {
      for (task <- Main.Tasks) {
        it(s"${task.Cmd} should have less than 80 characters per string for readability.") {
          for (line <- task.Doc.split("\n")) {
            withClue(line) {
              line.length should be < 80
            }
          }
        }
      }
    }
  }

  /** A helper method used to capture the console of a Cli execution and apply it to a partial function.
    * @param args
    *   String arguments to pass to the [[MultiTaskMain.go()]] method
    * @param pf
    *   A partial function to apply matchers
    * @tparam T
    *   The return value type of the thunk code to execute
    * @tparam U
    *   The return value type of the partial function to return.
    * @return
    *   The return value of the partial function.
    */
  def withGoMatching[T, U](args: Any*)(pf: scala.PartialFunction[(String, String), U]): U =
    withConsoleMatch(Main.go(args.map(_.toString): _*)) { case (_, stdout, stderr) =>
      pf(stdout, stderr)
    }

  /** A helper method used to capture the console of a Cli execution and return the output.
    *
    * @param args
    *   String arguments to pass to the [[MultiTaskMain.go()]] method
    * @return
    *   A tuple of the stdout and stderr
    */
  def withGo(args: Any*): (String, String) = withGoMatching(args: _*) { case any => any }

  /** A helper method used to capture the console of a Cli execution and return only stdout, ensuring that nothing is
    * written to stderr.
    *
    * @param args
    *   String arguments to pass to the [[MultiTaskMain.go()]] method
    * @return
    *   The stdout
    */
  def withGoStdout(args: Any*): String = withGoMatching(args: _*) { case (stdout, stderr) =>
    stderr shouldBe empty
    stdout
  }

  /** A helper method used to capture an exception thrown by [[withGo]]
    *
    * @param args
    *   String arguments to pass to the [[MultiTaskMain.go()]] method
    * @return
    *   The exception thrown when the arguments are run
    */
  def interceptGo[EX <: AnyRef](args: Any*)(implicit classTag: ClassTag[EX], pos: source.Position): EX =
    intercept[EX] { withGo(args: _*) }(classTag, pos)

  /** A helper method used to capture an [[DocoptException]] thrown by [[withGo]]
    *
    * @param args
    *   String arguments to pass to the [[MultiTaskMain.go()]] method
    * @return
    *   The exception thrown when the arguments are run
    */
  def interceptGoDocoptEx(args: Any*): DocoptException = interceptGo[DocoptException](args: _*)

  /** This helper allows the built-in test cases to be called using a string or string sequence. */
  class BuiltInAdapter(thunk: Seq[String] => Unit) extends Function[Seq[String], Unit] {
    def apply(): Unit = thunk(Seq.empty)
    def apply(in: String, in2: String*): Unit = thunk(in +: in2)
    override def apply(in: Seq[String]): Unit = thunk(in)
  }

  /** Run tests on the --help and --version flags that cause a system exit. */
  val itShouldHandleHelpAndVersionFlags: () => Unit = () => {
    it(s"throws an exception with '$TaskCmdPost--help'") {
      val t = interceptGoDocoptEx(TaskCmdArg :+ "--help": _*)
      // TODO: Specific subclass of DocoptException for help and version?
      t.getMessage shouldBe Doc
      t.exitCode shouldBe 0
    }

    it(s"throws an exception with '$TaskCmdPost--version'") {
      val t = interceptGoDocoptEx(TaskCmdArg :+ "--version": _*)
      t.getMessage shouldBe Main.Version
      t.exitCode shouldBe 0
    }

    it(s"throws an exception with no arguments") {
      val t = interceptGoDocoptEx(TaskCmdArg: _*)
      // TODO: This should have the help message, like --help above
      // TODO: This should always have the help text
      t.getMessage shouldBe Task.map(_ => null).getOrElse(Doc)
      t.exitCode shouldBe Task.map(_ => 1).getOrElse(0)
    }
  }

  /** When nothing is passed to a main, it acts as help. If a flag is passed but no other arguments, then it's missing
    * the command.
    */
  val itShouldThrowOnMissingTaskCommand: BuiltInAdapter = new BuiltInAdapter(args => {
    it("throw an exception like --help when run without a command") {
      val t = interceptGoDocoptEx(args: _*)
      t.getMessage shouldBe "Missing command"
      t.docopt shouldBe Main.Doc
    }
  })

  /** When a garbage command is passed. */
  val itShouldThrowOnUnknownTaskCommand: () => Unit = () => {
    it("throws an exception with an unknown command") {
      val t = intercept[DocoptException] { withGo(UnknownTxt) }
      t.getMessage shouldBe s"Unknown command: $UnknownTxt"
      t.docopt shouldBe Main.Doc
    }
  }

  /** Run tests on an unrecognized flag. */
  val itShouldThrowOnUnknownOptKey: () => Unit = () => {
    it("throws an exception with unknown option") {
      val t = interceptGoDocoptEx(TaskCmdArg :+ s"--$UnknownTxt": _*)
      t.docopt shouldBe Doc
      // TODO: This could be a better error message
      t.getMessage shouldBe null
    }
  }

  /** Run tests on a command line that is missing necessary information for the Cli to proceed. */
  val itShouldThrowOnIncompleteArgs: BuiltInAdapter = new BuiltInAdapter(args => {
    val allArgs = Task.map(_.Cmd).toSeq ++ args
    it("throws an exception on missing options: " + allArgs.mkString(" ")) {
      val t = interceptGoDocoptEx(allArgs: _*)
      t.docopt shouldBe Doc
      // TODO: This could be a better message
      t.getMessage shouldBe null
    }
  })

  /** Run tests on a command line where the last argument is an option missing its value. */
  val itShouldThrowOnMissingOptValue: BuiltInAdapter = new BuiltInAdapter(args => {
    val allArgs = Task.map(_.Cmd).toSeq ++ args
    it("throws an exception on missing option parameters: " + allArgs.mkString(" ")) {
      val t = interceptGoDocoptEx(allArgs: _*)
      t.exitCode shouldBe 1
      t.getMessage shouldBe s"${args.last} requires argument"
    }
  })

  /** Run tests on a command line with incompatible options. */
  val itShouldThrowOnIncompatibleOpts: BuiltInAdapter = new BuiltInAdapter(args => {
    val allArgs = Task.map(_.Cmd).toSeq ++ args
    it("throws an exception on incompatible arguments: " + allArgs.mkString(" ")) {
      val t = interceptGoDocoptEx(allArgs: _*)
      t.exitCode shouldBe 1
      // TODO: This could be a better message
      t.getMessage shouldBe null
    }
  })
}
