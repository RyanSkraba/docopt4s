package com.tinfoiled.docopt4s.testkit.example

import com.tinfoiled.docopt4s.testkit.MultiTaskMainSpec

/** Unit tests for [[DumpTask]] */
class DumpTaskSpec extends MultiTaskMainSpec(ExampleGo, Some(DumpTask)) {

  describe(s"Standard $MainName $TaskCmd command line help, versions and exceptions") {
    itShouldHandleHelpAndVersionFlags()
    itShouldThrowOnUnknownFlag()
    itShouldThrowOnIncompleteArgs()
    itShouldThrowOnIncompleteArgs("unknown")
    itShouldThrowOnMissingFlagValue("string", "arg", "--default")
    itShouldThrowOnIncompatibleOpts("string", "arg", "--options", "--default", "dflt")
  }

  describe("When running without any flags") {
    it("should work with a single argument") {
      withGoStdout(TaskCmd, "string", "x1") shouldBe "string ARGS:x1:--:<>\n"
    }
    it("should work with two arguments") {
      withGoStdout(TaskCmd, "string", "x1", "x2") shouldBe "string ARGS:x1:x2:<>\n"
    }
    it("should work with a three arguments") {
      withGoStdout(TaskCmd, "string", "x1", "x2", "x3") shouldBe "string ARGS:x1:x2:<x3>\n"
    }
    it("should work with four arguments") {
      withGoStdout(TaskCmd, "string", "x1", "x2", "x3", "x4") shouldBe "string ARGS:x1:x2:<x3,x4>\n"
    }
  }

  describe("When running with the --options flag") {
    it("should work with a single argument") {
      withGoStdout(TaskCmd, "string", "--options", "x1") shouldBe "string ARGS:Some(x1):None:Some(<>)\n"
    }
    it("should work with two arguments") {
      withGoStdout(TaskCmd, "string", "--options", "x1", "x2") shouldBe "string ARGS:Some(x1):Some(x2):Some(<>)\n"
    }
    it("should work with a three arguments") {
      withGoStdout(
        TaskCmd,
        "string",
        "--options",
        "x1",
        "x2",
        "x3"
      ) shouldBe "string ARGS:Some(x1):Some(x2):Some(<x3>)\n"
    }
    it("should work with four arguments") {
      withGoStdout(
        TaskCmd,
        "string",
        "--options",
        "x1",
        "x2",
        "x3",
        "x4"
      ) shouldBe "string ARGS:Some(x1):Some(x2):Some(<x3,x4>)\n"
    }
  }

  describe("When running with the --default flag") {
    it("should work with a single argument") {
      withGoStdout(TaskCmd, "string", "--default", "dflt", "x1") shouldBe "string ARGS:x1:dflt:<>\n"
    }
    it("should work with two arguments") {
      withGoStdout(TaskCmd, "string", "--default", "dflt", "x1", "x2") shouldBe "string ARGS:x1:x2:<>\n"
    }
    it("should work with a three arguments") {
      withGoStdout(TaskCmd, "string", "--default", "dflt", "x1", "x2", "x3") shouldBe "string ARGS:x1:x2:<x3>\n"
    }
    it("should work with four arguments") {
      withGoStdout(
        TaskCmd,
        "string",
        "--default",
        "dflt",
        "x1",
        "x2",
        "x3",
        "x4"
      ) shouldBe "string ARGS:x1:x2:<x3,x4>\n"
    }
  }
}
