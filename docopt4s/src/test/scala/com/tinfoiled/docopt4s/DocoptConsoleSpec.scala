package com.tinfoiled.docopt4s

import org.scalatest.BeforeAndAfterAll
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

/** Test the [[DocoptConsole]] helper. */
class DocoptConsoleSpec extends AnyFunSpecLike with BeforeAndAfterAll with Matchers {

  describe("Creating a console from Docopt flags") {
    it("should turn verbose off and on") {
      DocoptConsole(Docopt(Map())).cfg.verbose shouldBe false
      DocoptConsole(Docopt(Map("--verbose" -> true))).cfg.verbose shouldBe true
      DocoptConsole(Docopt(Map("--noVerbose" -> true))).cfg.verbose shouldBe false
      DocoptConsole(Docopt(Map("--verbose" -> true, "--noVerbose" -> false))).cfg.verbose shouldBe true
      DocoptConsole(Docopt(Map("--verbose" -> false, "--noVerbose" -> true))).cfg.verbose shouldBe false
      intercept[IncompatibleKeysException] {
        DocoptConsole(Docopt(Map("--verbose" -> true, "--noVerbose" -> true))).cfg.verbose shouldBe true
      }
    }

    it("should turn plain on") {
      DocoptConsole(Docopt(Map())).cfg.plain shouldBe false
      DocoptConsole(Docopt(Map("--plain" -> true))).cfg.plain shouldBe true
    }

    it("should turn yes mode on") {
      DocoptConsole(Docopt(Map())).cfg.yes shouldBe false
      DocoptConsole(Docopt(Map("--yes" -> true))).cfg.yes shouldBe true
    }
  }

  describe("Creating a console from Docopt custom flags") {
    val model = DocoptConsole(Docopt(Map()), "--x", "--nox", "--p", "--y")

    it("should turn verbose off and on") {
      model.copy(opt = Docopt(Map())).cfg.verbose shouldBe false
      model.copy(opt = Docopt(Map("--x" -> true))).cfg.verbose shouldBe true
      model.copy(opt = Docopt(Map("--nox" -> true))).cfg.verbose shouldBe false
      model.copy(opt = Docopt(Map("--x" -> true, "--nox" -> false))).cfg.verbose shouldBe true
      model.copy(opt = Docopt(Map("--x" -> false, "--nox" -> true))).cfg.verbose shouldBe false
      intercept[IncompatibleKeysException] {
        model.copy(opt = Docopt(Map("--x" -> true, "--nox" -> true))).cfg.verbose shouldBe true
      }
    }

    it("should turn plain on") {
      model.copy(opt = Docopt(Map())).cfg.plain shouldBe false
      model.copy(opt = Docopt(Map("--p" -> true))).cfg.plain shouldBe true
    }

    it("should turn yes mode on") {
      model.copy(opt = Docopt(Map())).cfg.yes shouldBe false
      model.copy(opt = Docopt(Map("--y" -> true))).cfg.yes shouldBe true
    }
  }
}
