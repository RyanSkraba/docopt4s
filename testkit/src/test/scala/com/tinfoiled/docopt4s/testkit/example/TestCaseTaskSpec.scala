package com.tinfoiled.docopt4s.testkit.example

import com.tinfoiled.docopt4s.DocoptException
import com.tinfoiled.docopt4s.testkit.{MultiTaskMainSpec, TmpDir}
import com.tinfoiled.docopt4s.testkit.example.TestCaseTask.TestCase
import com.tinfoiled.docopt4s.testkit.example.TestCaseTask.TestCase._

import java.nio.file.Files
import scala.io.Source
import scala.util.{Failure, Success, Using}

/** Unit tests for [[DumpTask]] */
class TestCaseTaskSpec extends MultiTaskMainSpec(ExampleGo, Some(TestCaseTask)) with TmpDir {

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

      it("should fail with one argument") { interceptWrapped(SimpleDocopt :+ "--check={}" :+ "x") }
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

  describe("Running a test case from a file") {

    describe("The TestCase.splitArgs helper method") {
      for (in <- Seq("a b c", "   a b c", "a    b c", "a b    c", "a\tb    \n\nc ")) {
        it(s"should ignore whitespace: $in") {
          splitArgs(in) shouldBe Seq("a", "b", "c")
        }
      }

      it("should accept double quotes") {
        splitArgs(raw"""a1 "b c" d""") shouldBe Seq("a1", "b c", "d")
        splitArgs(raw"""a2 "b c d""") shouldBe Seq("a2", "\"b", "c", "d")
        splitArgs(raw"""a3 "b\"c" d""") shouldBe Seq("a3", raw"""b"c""", "d")
        splitArgs(raw"""a4 "b\\"c" d""") shouldBe Seq("a4", "b\\", "c\"", "d")
        splitArgs(raw"""a5 "b\\\"c" d""") shouldBe Seq("a5", raw"""b\"c""", "d")
        splitArgs(raw"""a6 "b
                       |\\nc" d""".stripMargin) shouldBe Seq("a6", "b\n\\nc", "d")
      }
    }

    val q3 = "\"\"\""

    it("should use the execute method to run a test successfully") {
      TestCase("Usage: program ARG1", ujson.Obj("ARG1" -> "a1"), Seq("a1")).execute() shouldBe Success(
        ujson.Obj("ARG1" -> "a1")
      )
    }

    it("should use the execute method to run a test with unexpected results") {
      val ex = TestCase("Usage: program ARG1", ujson.Obj("ARG1" -> "a2"), Seq("a1")).execute().failed.get
      ex shouldBe a[AssertionError]
      ex.getMessage shouldBe """{"ARG1":"a1"}"""
    }

    it("should use the execute method to check a failure") {
      val ex = TestCase("Usage: program ARG1 ARG2", ujson.Obj("ARG1" -> "a1"), Seq("a1")).execute().failed.get
      ex shouldBe a[DocoptException]
      ex.getMessage shouldBe null
    }

    it("should parse an input file into test cases") {
      TestCase.parse(s"""
          |r${q3}A$q3
          |a1
          |{"a2": true}
          |
          |r$q3
          |B
          |$q3
          |b1
          |{"b2": true}
          |b3
          |{"b4": true}
          |
          |${q3}C
          |$q3
          |$$ prog c1
          |{"c2": true}
          |c3
          |""".stripMargin) shouldBe
        Seq(
          TestCase("A", ujson.Obj("a2" -> true), Seq("a1")),
          TestCase("\nB\n", ujson.Obj("b2" -> true), Seq("b1")),
          TestCase("\nB\n", ujson.Obj("b4" -> true), Seq("b3")),
          TestCase("C\n", ujson.Obj("c2" -> true), Seq("c1"))
        )
    }

    it("should run from the command line with --file") {
      val file = Tmp.resolve("filearg.docopt")
      Files.writeString(
        file,
        Using.resource(Source.fromInputStream(getClass.getResourceAsStream("basic.docopt"))) {
          _.getLines().mkString("\n")
        }
      )
      withGoStdout(TaskCmd, s"--file", file) shouldBe empty
    }

    for (file <- Seq("basic", "testcases"))
      describe(s"with $file.docopt") {
        val tests =
          TestCase.parse(Using.resource(Source.fromInputStream(getClass.getResourceAsStream(s"$file.docopt"))) {
            _.getLines().mkString("\n")
          })
        for ((tc, i) <- tests.zipWithIndex) {
          it(s"should test ($i)") {
            tc.execute() match {
              case Success(_)                                     => succeed
              case Failure(ex) if ex.isInstanceOf[AssertionError] => ex.getMessage shouldBe tc.expected
              case Failure(ex)                                    => fail(ex)
            }
          }
        }
      }
  }
}
