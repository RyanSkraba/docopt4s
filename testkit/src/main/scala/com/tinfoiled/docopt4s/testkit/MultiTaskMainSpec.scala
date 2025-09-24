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

  /** The command used to specify the task (if present) or the empty string if not. */
  lazy val TaskCmd: String = Task.map(_.Cmd).getOrElse("")

  /** A flag that doesn't exist in the Docopt. */
  lazy val UnknownFlag: String = "--unknownDoesNotExistGarbage"

  describe(Main.Name + Task.map(" " + _.Cmd).getOrElse("") + " docopt check") {
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

  /** Run tests on the --help and --version flags that cause a system exit. */
  val itShouldThrowOnHelpAndVersionFlags: () => Unit = () => {

    val prefixArgs = Task.map(_.Cmd).toSeq

    it(s"throws an exception with ${prefixArgs.mkString(" ")} --help") {
      val t = interceptGoDocoptEx(prefixArgs :+ "--help": _*)
      // TODO: Specific subclass of DocoptException for help and version?
      t.getMessage shouldBe Doc
      t.exitCode shouldBe 0
    }

    it(s"throws an exception with ${prefixArgs.mkString(" ")} --version") {
      val t = interceptGoDocoptEx(prefixArgs :+ "--version": _*)
      t.getMessage shouldBe Main.Version
      t.exitCode shouldBe 0
    }

    it(s"throws an exception with a bare ${prefixArgs.mkString(" ")}") {
      val t = interceptGoDocoptEx(prefixArgs: _*)
      // TODO: This should have the help message, like --help above
      // TODO: This should always have the help text
      t.getMessage shouldBe Task.map(_ => null).getOrElse(Doc)
      t.exitCode shouldBe Task.map(_ => 1).getOrElse(0)
    }
  }

  /** Run tests on an unrecognized flag. */
  val itShouldThrowOnUnknownFlag: () => Unit = () => {
    val prefixArgs = Task.map(_.Cmd).toSeq
    it("throws an exception with unknown option") {
      val t = interceptGoDocoptEx(prefixArgs :+ UnknownFlag: _*)
      t.docopt shouldBe Doc
      // TODO: This could be a better error message
      t.getMessage shouldBe null
    }
  }

  /** Run tests on a command line that is missing necessary information for the Cli to proceed. */
  val itShouldThrowOnIncompleteArgs: Seq[String] => Unit = args => {
    val allArgs = Task.map(_.Cmd).toSeq ++ args
    it("throws an exception on missing options: " + allArgs.mkString(" ")) {
      val t = interceptGoDocoptEx(allArgs: _*)
      t.docopt shouldBe Doc
      // TODO: This could be a better message
      t.getMessage shouldBe null
    }
  }

  /** Run tests on a command line where the last argument is an option missing its value. */
  val itShouldThrowOnMissingFlagValue: Seq[String] => Unit = args => {
    val allArgs = Task.map(_.Cmd).toSeq ++ args
    it("throws an exception on missing option parameters: " + allArgs.mkString(" ")) {
      val t = interceptGoDocoptEx(allArgs: _*)
      t.exitCode shouldBe 1
      t.getMessage shouldBe s"${args.last} requires argument"
    }
  }

  /** Run tests on a command line with incompatible options. */
  val itShouldThrowOnIncompatibleOpts: Seq[String] => Unit = args => {
    val allArgs = Task.map(_.Cmd).toSeq ++ args
    it("throws an exception on incompatible arguments: " + allArgs.mkString(" ")) {
      val t = interceptGoDocoptEx(allArgs: _*)
      t.exitCode shouldBe 1
      // TODO: This could be a better message
      t.getMessage shouldBe null
    }
  }
}
