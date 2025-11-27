package com.tinfoiled.docopt4s.testkit.example

import com.tinfoiled.docopt4s.testkit.MultiTaskMainSpec

/** Unit tests for [[NavalFateTask]] */
class NavalFateTaskSpec extends MultiTaskMainSpec(ExampleGo, Some(NavalFateTask)) {

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

  describe(s"Standard $MainName $TaskCmd command line help, versions and exceptions") {
    itShouldHandleVersionAndHelpFlags()
    itShouldThrowOnUnknownOptKey()
    itShouldThrowOnIncompleteArgs()
    itShouldThrowOnIncompleteArgs("ship")
    itShouldThrowOnIncompleteArgs("ship", "new")
    itShouldThrowOnIncompleteArgs("ship", "titanic")
    itShouldThrowOnIncompleteArgs("ship", "titanic", "move")
    itShouldThrowOnMissingOptValue("ship", "titanic", "move", "--speed")
    itShouldThrowOnIncompleteArgs("ship", "titanic", "move", "--speed", "0")
    itShouldThrowOnIncompleteArgs("ship", "titanic", "move", "0")
    itShouldThrowOnMissingOptValue("ship", "titanic", "move", "0", "0", "--speed")
    itShouldThrowOnMissingOptValue("ship", "titanic", "move", "0", "--speed")
    itShouldThrowOnIncompleteArgs("ship", "shoot")
    itShouldThrowOnIncompleteArgs("ship", "shoot", "0")
    itShouldThrowOnIncompatibleOpts("mine", "set", "0", "0", "--moored", "--drifting")
  }

  describe("When running the ship sub-subtask ") {

    it("should accept 'ship new' with name arguments") {
      // These three are the same, just different ways to pass to withGoStdOut
      withGoStdout(TaskCmd, "ship", "new", "titanic") shouldBe "Creating ship: titanic\n"
      withGoStdout(TaskCmd, Seq("ship", "new", "titanic")) shouldBe "Creating ship: titanic\n"
      withGoStdout(TaskCmd, "ship" -> "new", "titanic") shouldBe "Creating ship: titanic\n"

      withGoStdout(TaskCmd, "ship", "new", "titanic", "minnow") shouldBe
        """Creating ship: titanic
            |Creating ship: minnow
            |""".stripMargin
    }

    it("should accept 'ship <name> move' with the default speed") {
      withGoStdout(TaskCmd, "ship", "titanic", "move", "123", "987") shouldBe
        """Moving titanic ship
            |  to coordinates (123, 987)
            |  at speed 10
            |""".stripMargin
    }

    it("should accept 'ship <name> move' with a specific speed") {
      withGoStdout(TaskCmd, "ship", "titanic", "move", "123", "987", "--speed", "-123") shouldBe
        """Moving titanic ship
            |  to coordinates (123, 987)
            |  at speed -123
            |""".stripMargin
    }

    it("should accept the mining commands") {
      withGoStdout(TaskCmd, "mine", "set", "10", "20") shouldBe
        """Setting a mine
          |  at coordinates (10, 20)
          |""".stripMargin
      withGoStdout(TaskCmd, "mine", "remove", "10", "20") shouldBe
        """Removing a mine
          |  at coordinates (10, 20)
          |""".stripMargin
      withGoStdout(TaskCmd, "mine", "set", "10", "20", "--drifting") shouldBe
        """Setting a drifting mine
          |  at coordinates (10, 20)
          |""".stripMargin
      withGoStdout(TaskCmd, "mine", "remove", "10", "20", "--moored") shouldBe
        """Removing a moored mine
          |  at coordinates (10, 20)
          |""".stripMargin
      // Note the partial flag
      withGoStdout(TaskCmd, "mine", "remove", "10", "20", "--dri") shouldBe
        """Removing a drifting mine
          |  at coordinates (10, 20)
          |""".stripMargin
    }
  }
}
