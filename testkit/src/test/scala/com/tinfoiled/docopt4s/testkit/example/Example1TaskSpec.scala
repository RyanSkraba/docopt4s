package com.tinfoiled.docopt4s.testkit.example

import com.tinfoiled.docopt4s.testkit.MultiTaskMainSpec

/** Unit tests for [[Example1Task]] */
class Example1TaskSpec extends MultiTaskMainSpec(ExampleGo, Some(Example1Task)) {

  describe(s"${Main.Name} $TaskCmd command line errors") {
    itShouldThrowOnHelpAndVersionFlags()
    itShouldThrowOnUnknownFlag()
    itShouldThrowOnMissingOpt(Seq.empty)
    itShouldThrowOnMissingOptValue(Seq("--default"))
  }

  describe("When running with arguments") {
    it("should work on a single basic example") {
      withGoMatching(TaskCmd, "arg1") { case (stdout, stderr) =>
        stderr shouldBe empty
        stdout shouldBe
          """Command:example1
            |--options:false
            |--default:None
            |ARG1:arg1
            |ARG2:0
            |ARG3:()
            |""".stripMargin
      }
    }
  }
}
