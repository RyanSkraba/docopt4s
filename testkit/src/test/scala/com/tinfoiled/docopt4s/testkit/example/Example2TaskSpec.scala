package com.tinfoiled.docopt4s.testkit.example

import com.tinfoiled.docopt4s.testkit.MultiTaskMainSpec

/** Unit tests for [[Example2Task]] */
class Example2TaskSpec extends MultiTaskMainSpec(ExampleGo, Some(Example2Task)) {

  /*
  """$Description
     |
     |Usage:
     |  ${ExampleGo.Name} $Cmd ship new <name>...
     |  ${ExampleGo.Name} $Cmd ship <name> move <x> <y> [--speed=<kn>]
     |  ${ExampleGo.Name} $Cmd ship shoot <x> <y>
     |  ${ExampleGo.Name} $Cmd mine (set|remove) <x> <y> [--moored|--drifting]
     |  ${ExampleGo.Name} $Cmd -h | --help
     |  ${ExampleGo.Name} $Cmd --version
     |
     |Options:
     |  -h --help     Show this screen.
     |  --version     Show version.
     |  --speed=<kn>  Speed in knots [default: 10].
     |  --moored      Moored (anchored) mine.
     |  --drifting    Drifting mine.
     |
     |""".stripMargin.trim
   */

  describe(s"Standard ${Main.Name} $TaskCmd command line help, versions and exceptions") {
    itShouldThrowOnHelpAndVersionFlags()
    itShouldThrowOnUnknownFlag()
    itShouldThrowOnMissingOpt(Seq.empty)
    itShouldThrowOnMissingOpt(Seq("ship"))
    itShouldThrowOnMissingOpt(Seq("ship", "new"))
    itShouldThrowOnMissingOpt(Seq("ship", "titanic"))
    itShouldThrowOnMissingOpt(Seq("ship", "titanic", "move"))
    itShouldThrowOnMissingOptValue(Seq("ship", "titanic", "move", "--speed"))
    itShouldThrowOnMissingOpt(Seq("ship", "titanic", "move", "--speed", "0"))
    itShouldThrowOnMissingOpt(Seq("ship", "titanic", "move", "0"))
    itShouldThrowOnMissingOptValue(Seq("ship", "titanic", "move", "0", "0", "--speed"))
    itShouldThrowOnMissingOptValue(Seq("ship", "titanic", "move", "0", "--speed"))
    itShouldThrowOnMissingOpt(Seq("ship", "shoot"))
    itShouldThrowOnMissingOpt(Seq("ship", "shoot", "0"))
    itShouldThrowOnIncompatibleOpts(Seq("mine", "set", "0", "0", "--moored", "--drifting"))
  }

  describe("When running the ship sub-subtask ") {

    it("should accept 'ship new' with name arguments") {
      withGoMatching(TaskCmd, "ship", "new", "titanic") { case (stdout, stderr) =>
        stderr shouldBe empty
        stdout shouldBe
          """Creating ship: titanic
            |""".stripMargin
      }
      withGoMatching(TaskCmd, "ship", "new", "titanic", "minnow") { case (stdout, stderr) =>
        stderr shouldBe empty
        stdout shouldBe
          """Creating ship: titanic
            |Creating ship: minnow
            |""".stripMargin
      }
    }

    it("should accept 'ship <name> move' with the default speed") {
      withGoMatching(TaskCmd, "ship", "titanic", "move", "123", "987") { case (stdout, stderr) =>
        stderr shouldBe empty
        stdout shouldBe
          """Moving titanic ship
            |  to coordinates (123, 987)
            |  at speed 10
            |""".stripMargin
      }
    }

    it("should accept 'ship <name> move' with a specific speed") {
      withGoMatching(TaskCmd, "ship", "titanic", "move", "123", "987", "--speed", "-123") { case (stdout, stderr) =>
        stderr shouldBe empty
        stdout shouldBe
          """Moving titanic ship
            |  to coordinates (123, 987)
            |  at speed -123
            |""".stripMargin
      }
    }
  }
}
