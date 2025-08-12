package com.tinfoiled.docopt4s.testkit.example

import com.tinfoiled.docopt4s.testkit.MultiTaskMainSpec

/** Unit tests for [[DumpTask]] */
class TestCaseTaskSpec extends MultiTaskMainSpec(ExampleGo, Some(TestCaseTask)) {

  describe(s"Standard ${Main.Name} $TaskCmd command line help, versions and exceptions") {
    itShouldThrowOnHelpAndVersionFlags()
    itShouldThrowOnUnknownFlag()
    itShouldThrowOnIncompleteArgs(Seq("unknown"))
    itShouldThrowOnIncompleteArgs(Seq("--docopt", "doc"))
    itShouldThrowOnMissingFlagValue(Seq("--docopt"))
  }

  describe("Running a basic test case") {
    it("should work with a single argument") {
      withGoStdout(TaskCmd, "--docopt", "Usage: program", "") shouldBe "{}"
    }
  }
}
