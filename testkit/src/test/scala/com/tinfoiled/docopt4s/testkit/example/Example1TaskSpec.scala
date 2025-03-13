package com.tinfoiled.docopt4s.testkit.example

import com.tinfoiled.docopt4s.testkit.MultiTaskMainSpec

/** Unit tests for [[Example1Task]] */
class Example1TaskSpec extends MultiTaskMainSpec(ExampleGo, Some(Example1Task)) {

  describe(s"${Main.Name} $TaskCmd command line errors") {
    itShouldThrowOnHelpAndVersionFlags()
    itShouldThrowOnUnknownFlag()
    itShouldThrowOnMissingOpt(Seq.empty)
    itShouldThrowOnMissingOptValue(Seq("--default"))
    itShouldThrowOnIncompatibleOpts(Seq("--options", "--default", "dflt"))
  }

  describe("When running without any flags") {
    it("should work with a single argument") {
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
    it("should work with two arguments") {
      withGoMatching(TaskCmd, "arg1", "2") { case (stdout, stderr) =>
        stderr shouldBe empty
        stdout shouldBe
          """Command:example1
            |--options:false
            |--default:None
            |ARG1:arg1
            |ARG2:2
            |ARG3:()
            |""".stripMargin
      }
    }
    it("should work with a three arguments") {
      withGoMatching(TaskCmd, "arg1", "3", "arg3") { case (stdout, stderr) =>
        stderr shouldBe empty
        stdout shouldBe
          """Command:example1
            |--options:false
            |--default:None
            |ARG1:arg1
            |ARG2:3
            |ARG3:(arg3)
            |""".stripMargin
      }
    }
    it("should work with four arguments") {
      withGoMatching(TaskCmd, "arg1", "4", "arg3", "arg4") { case (stdout, stderr) =>
        stderr shouldBe empty
        stdout shouldBe
          """Command:example1
            |--options:false
            |--default:None
            |ARG1:arg1
            |ARG2:4
            |ARG3:(arg3,arg4)
            |""".stripMargin
      }
    }
  }

  describe("When running with the --options flag") {
    it("should work with a single argument") {
      withGoMatching(TaskCmd, "--options", "arg1") { case (stdout, stderr) =>
        stderr shouldBe empty
        stdout shouldBe
          """Command:example1
            |--options:Some(true)
            |--default:None
            |ARG1:Some(arg1)
            |ARG2:None
            |ARG3:Some(())
            |""".stripMargin
      }
    }
    it("should work with two arguments") {
      withGoMatching(TaskCmd, "--options", "arg1", "2") { case (stdout, stderr) =>
        stderr shouldBe empty
        stdout shouldBe
          """Command:example1
            |--options:Some(true)
            |--default:None
            |ARG1:Some(arg1)
            |ARG2:Some(2)
            |ARG3:Some(())
            |""".stripMargin
      }
    }
    it("should work with a three arguments") {
      withGoMatching(TaskCmd, "--options", "arg1", "3", "arg3") { case (stdout, stderr) =>
        stderr shouldBe empty
        stdout shouldBe
          """Command:example1
            |--options:Some(true)
            |--default:None
            |ARG1:Some(arg1)
            |ARG2:Some(3)
            |ARG3:Some((arg3))
            |""".stripMargin
      }
    }
    it("should work with four arguments") {
      withGoMatching(TaskCmd, "--options", "arg1", "4", "arg3", "arg4") { case (stdout, stderr) =>
        stderr shouldBe empty
        stdout shouldBe
          """Command:example1
            |--options:Some(true)
            |--default:None
            |ARG1:Some(arg1)
            |ARG2:Some(4)
            |ARG3:Some((arg3,arg4))
            |""".stripMargin
      }
    }
  }
}
