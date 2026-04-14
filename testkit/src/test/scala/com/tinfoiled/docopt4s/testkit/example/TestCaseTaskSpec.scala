package com.tinfoiled.docopt4s.testkit.example

import com.tinfoiled.docopt4s.DocoptException
import com.tinfoiled.docopt4s.testkit.MultiTaskMainSpec

import scala.util.{Failure, Success}

/** Unit tests for [[DumpTask]] */
class TestCaseTaskSpec extends MultiTaskMainSpec(ExampleGo, Some(TestCaseTask)) {

  describe(s"Standard $MainName $TaskCmd command line help, versions and exceptions") {
    itShouldHandleVersionAndHelpFlags()
    itShouldThrowOnUnknownOptKey()
    itShouldThrowOnIncompleteArgs("unknown")
    itShouldThrowOnIncompleteArgs("--docopt", "doc")
    itShouldThrowOnMissingOptValue("--docopt", "doc", "--keys")
    itShouldThrowOnMissingOptValue("--docopt", "doc", "--check")
  }

  /** Run the test and unwrap the exception that happened internally. */
  def interceptWrapped(args: Seq[String]): DocoptException = {
    val t = interceptGoDocoptEx(args: _*)
    t.docopt shouldBe Doc
    t.exitCode shouldBe 1
    t.getMessage shouldBe null
    t.getCause shouldBe a[DocoptException]
    // Get the wrapped exception
    val wrapped = t.getCause.asInstanceOf[DocoptException]
    Option(wrapped.docopt) shouldBe args.sliding(2).find(_.head == "--docopt").map(_(1))
    wrapped.exitCode shouldBe 1
    wrapped.getMessage shouldBe null // TODO: Improve errors
    wrapped
  }

  describe("The tokenize helper method") {
    for (in <- Seq("a b c", "   a b c", "a    b c", "a b    c", "a\tb    \n\nc ")) {
      it(s"should ignore whitespace: $in") {
        TestCaseTask.tokenize(in) shouldBe Seq("a", "b", "c")
      }
    }

    it("should accept double quotes") {
      TestCaseTask.tokenize(raw"""a1 "b c" d""") shouldBe Seq("a1", "b c", "d")
      TestCaseTask.tokenize(raw"""a2 "b c d""") shouldBe Seq("a2", "\"b", "c", "d")
      TestCaseTask.tokenize(raw"""a3 "b\"c" d""") shouldBe Seq("a3", raw"""b"c""", "d")
      TestCaseTask.tokenize(raw"""a4 "b\\"c" d""") shouldBe Seq("a4", "b\\", "c\"", "d")
      TestCaseTask.tokenize(raw"""a5 "b\\\"c" d""") shouldBe Seq("a5", raw"""b\"c""", "d")
      TestCaseTask.tokenize(raw"""a6 "b
             |\\nc" d""".stripMargin) shouldBe Seq("a6", "b\n\\nc", "d")
    }
  }

  describe("Running a test case on an empty program") {
    val SimpleDocopt = Seq(TaskCmd, "--docopt", "Usage: program")

    describe("using --keys") {
      it("should work with no arguments") { withGoStdout(SimpleDocopt :+ "--keys=": _*) shouldBe "{}" }
      it("should fail with one argument") { interceptWrapped(SimpleDocopt :+ "--keys=" :+ "unknown") }
    }

    describe("using --check") {
      it("should work with no arguments") { withGoStdout(SimpleDocopt :+ "--check={}": _*) shouldBe "OK" }

      it("should work with no arguments but report unexpected results") {
        withGoMatching(SimpleDocopt :+ """--check={"x": ""}""": _*) { case (stdout, stderr) =>
          stderr shouldBe """NOK: {} != {"x": ""}"""
          stdout shouldBe empty
        }
      }

      it("should fail with one argument") { interceptWrapped(SimpleDocopt :+ "--check=" :+ "unknown") }
    }
  }

  describe("Running a test case on an basic program") {
    val BasicDocopt = Seq(TaskCmd, "--docopt", "Usage: program [--flag] CMD ARGS...")

    describe("using --keys") {
      it("should fail with no arguments") { interceptWrapped(BasicDocopt :+ "--keys=--flag,CMD,ARGS") }

      it("should fail with one argument") { interceptWrapped(BasicDocopt :+ "--keys=--flag,CMD,ARGS" :+ "a1") }

      it("should work with two arguments") {
        withGoStdout(BasicDocopt ++ Seq("--keys=--flag,CMD,ARGS", "a1", "a2"): _*) shouldBe
          """{"--flag":"false","CMD":"a1","ARGS":"a2"}"""
      }

      ignore("should work with three arguments and a flag") {
        withGoStdout(BasicDocopt ++ Seq("--keys=--flag,CMD,ARGS", "--flag", "a1", "a2", "a3"): _*) shouldBe
          """{"--flag":"true","CMD":"a1","ARGS":["a2","a3"]}"""
      }
    }

    describe("using --check") {
      it("should fail with no arguments") { interceptWrapped(BasicDocopt :+ "--check={}") }

      it("should fail with one argument") { interceptWrapped(BasicDocopt :+ "--check={}" :+ "a1") }

      it("should work with two arguments") {
        withGoStdout(
          BasicDocopt ++ Seq("""--check={"--flag":"false","CMD":"a1","ARGS":"a2"}""", "a1", "a2"): _*
        ) shouldBe "OK"
      }

      it("should work with two arguments but report unexpected results") {
        withGoMatching(BasicDocopt ++ Seq("""--check={"ARGS":"a1"}""", "a1", "a2"): _*) { case (stdout, stderr) =>
          stderr shouldBe """NOK: {"ARGS":"a2"} != {"ARGS":"a1"}"""
          stdout shouldBe empty
        }
      }

      it("should work with five arguments (like the TestCaseTask docopt)") {
        withGoStdout(
          BasicDocopt ++ Seq(
            """--check={"--flag":"false","CMD":"a1","ARGS":["a2","a3","a4","a5"]}""",
            "a1",
            "a2",
            "a3",
            "a4",
            "a5"
          ): _*
        ) shouldBe "OK"
      }
    }
  }

  describe("Running a file-based test") {
    it("should use the checkTest method to run a test successfully") {
      TestCaseTask.checkTest("Usage: program ARG1", Seq("a1"), """{"ARG1":"a1"}""") shouldBe Success(
        """{"ARG1":"a1"}"""
      )
    }

    it("should use the checkTest method to run a test with unexpected results") {
      val ex = TestCaseTask.checkTest("Usage: program ARG1", Seq("a1"), """{"ARG1":"a2"}""")
      ex shouldBe a[Failure[_]]
      ex.failed.get.getMessage shouldBe """{"ARG1":"a1"}"""
    }

    ignore("should use the checkTest method to check a failure") {
      TestCaseTask.checkTest("Usage: program ARG1", Seq("a1", "a2"), """{""ARG1":"a2"}""")
    }
  }
}
