package com.tinfoiled.docopt4s.testkit.example

import com.tinfoiled.docopt4s.testkit.MultiTaskMainSpec

/** Unit tests for [[DumpTask]] */
class TestCaseTaskSpec extends MultiTaskMainSpec(ExampleGo, Some(TestCaseTask)) {

  describe(s"Standard $MainName $TaskCmd command line help, versions and exceptions") {
    itShouldHandleHelpAndVersionFlags()
    itShouldThrowOnUnknownFlag()
    itShouldThrowOnIncompleteArgs("unknown")
    itShouldThrowOnIncompleteArgs("--docopt", "doc")
    itShouldThrowOnMissingFlagValue("--docopt")
  }

  describe("Running a basic test case") {
    it("should work with a single argument") {
      withGoStdout(TaskCmd, "--docopt", "Usage: program", "") shouldBe "{}"
    }
  }
}
