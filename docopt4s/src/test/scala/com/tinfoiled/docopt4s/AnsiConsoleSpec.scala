package com.tinfoiled.docopt4s

import com.tinfoiled.docopt4s.AnsiConsole.withConsoleMatch
import org.scalatest.BeforeAndAfterAll
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

import java.io.ByteArrayInputStream
import scala.io.AnsiColor._
import scala.reflect.io.Streamable

/** Test the [[AnsiConsole]] helper. */
class AnsiConsoleSpec extends AnyFunSpecLike with BeforeAndAfterAll with Matchers {

  describe("Providing ANSI colours") {

    describe("via the style method") {
      it("controls whether bold, reset and colour control codes are added") {
        val cfg = AnsiConsole()
        cfg.style("-") shouldBe s"$WHITE-$RESET"
        cfg.style("-", bold = false, reset = false) shouldBe s"$WHITE-"
        cfg.style("-", bold = false, reset = true) shouldBe s"$WHITE-$RESET"
        cfg.style("-", bold = true, reset = false) shouldBe s"$BOLD$WHITE-"
        cfg.style("-", bold = true, reset = true) shouldBe s"$BOLD$WHITE-$RESET"
        cfg.style("-", "", "*") shouldBe s"*-$RESET"
        cfg.style("-", "", "*", bold = false, reset = false) shouldBe s"*-"
        cfg.style("-", "", "*", bold = false, reset = true) shouldBe s"*-$RESET"
        cfg.style("-", "", "*", bold = true, reset = false) shouldBe s"$BOLD*-"
        cfg.style(
          "-",
          "",
          "*",
          bold = true,
          reset = true
        ) shouldBe s"$BOLD*-$RESET"
        cfg.style("-", "x") shouldBe s"$BOLD$WHITE-$RESET$WHITE x$RESET"
        cfg.style(
          "-",
          "x",
          bold = false,
          reset = false
        ) shouldBe s"$BOLD$WHITE-$RESET$WHITE x"
        cfg.style(
          "-",
          "x",
          bold = false,
          reset = true
        ) shouldBe s"$BOLD$WHITE-$RESET$WHITE x$RESET"
        cfg.style(
          "-",
          "x",
          bold = true,
          reset = false
        ) shouldBe s"$BOLD$WHITE- x"
        cfg.style(
          "-",
          "x",
          bold = true,
          reset = true
        ) shouldBe s"$BOLD$WHITE- x$RESET"
      }

      it("disables control codes") {
        val cfg = AnsiConsole(plain = true)
        cfg.style("-") shouldBe "-"
        cfg.style("-", bold = false, reset = false) shouldBe "-"
        cfg.style("-", bold = false, reset = true) shouldBe "-"
        cfg.style("-", bold = true, reset = false) shouldBe "-"
        cfg.style("-", bold = true, reset = true) shouldBe "-"
        cfg.style("-", "", "*") shouldBe "-"
        cfg.style("-", "", "*", bold = false, reset = false) shouldBe "-"
        cfg.style("-", "", "*", bold = false, reset = true) shouldBe "-"
        cfg.style("-", "", "*", bold = true, reset = false) shouldBe "-"
        cfg.style("-", "", "*", bold = true, reset = true) shouldBe "-"
        cfg.style("-", "x", "*") shouldBe "- x"
        cfg.style("-", "x", "*", bold = false, reset = false) shouldBe "- x"
        cfg.style("-", "x", "*", bold = false, reset = true) shouldBe "- x"
        cfg.style("-", "x", "*", bold = true, reset = false) shouldBe "- x"
        cfg.style("-", "x", "*", bold = true, reset = true) shouldBe "- x"
      }
    }

    describe("via colour constants") {
      it("should be populated when activated") {
        val cfg = AnsiConsole()

        cfg.Black shouldBe BLACK
        cfg.Red shouldBe RED
        cfg.Green shouldBe GREEN
        cfg.Yellow shouldBe YELLOW
        cfg.Blue shouldBe BLUE
        cfg.Magenta shouldBe MAGENTA
        cfg.Cyan shouldBe CYAN
        cfg.White shouldBe WHITE

        cfg.HiBlack shouldBe BLACK.replace("[3", "[9")
        cfg.HiRed shouldBe RED.replace("[3", "[9")
        cfg.HiGreen shouldBe GREEN.replace("[3", "[9")
        cfg.HiYellow shouldBe YELLOW.replace("[3", "[9")
        cfg.HiBlue shouldBe BLUE.replace("[3", "[9")
        cfg.HiMagenta shouldBe MAGENTA.replace("[3", "[9")
        cfg.HiCyan shouldBe CYAN.replace("[3", "[9")
        cfg.HiWhite shouldBe WHITE.replace("[3", "[9")

        cfg.BlackBg shouldBe BLACK_B
        cfg.RedBg shouldBe RED_B
        cfg.GreenBg shouldBe GREEN_B
        cfg.YellowBg shouldBe YELLOW_B
        cfg.BlueBg shouldBe BLUE_B
        cfg.MagentaBg shouldBe MAGENTA_B
        cfg.CyanBg shouldBe CYAN_B
        cfg.WhiteBg shouldBe WHITE_B

        cfg.HiBlackBg shouldBe BLACK.replace("[3", "[10")
        cfg.HiRedBg shouldBe RED.replace("[3", "[10")
        cfg.HiGreenBg shouldBe GREEN.replace("[3", "[10")
        cfg.HiYellowBg shouldBe YELLOW.replace("[3", "[10")
        cfg.HiBlueBg shouldBe BLUE.replace("[3", "[10")
        cfg.HiMagentaBg shouldBe MAGENTA.replace("[3", "[10")
        cfg.HiCyanBg shouldBe CYAN.replace("[3", "[10")
        cfg.HiWhiteBg shouldBe WHITE.replace("[3", "[10")

        cfg.Bold shouldBe BOLD
        cfg.Reset shouldBe RESET
      }

      it("should be empty when deactivated") {
        val cfg = AnsiConsole(plain = true)

        cfg.Black shouldBe empty
        cfg.Red shouldBe empty
        cfg.Green shouldBe empty
        cfg.Yellow shouldBe empty
        cfg.Blue shouldBe empty
        cfg.Magenta shouldBe empty
        cfg.Cyan shouldBe empty
        cfg.White shouldBe empty

        cfg.HiBlack shouldBe empty
        cfg.HiRed shouldBe empty
        cfg.HiGreen shouldBe empty
        cfg.HiYellow shouldBe empty
        cfg.HiBlue shouldBe empty
        cfg.HiMagenta shouldBe empty
        cfg.HiCyan shouldBe empty
        cfg.HiWhite shouldBe empty

        cfg.BlackBg shouldBe empty
        cfg.RedBg shouldBe empty
        cfg.GreenBg shouldBe empty
        cfg.YellowBg shouldBe empty
        cfg.BlueBg shouldBe empty
        cfg.MagentaBg shouldBe empty
        cfg.CyanBg shouldBe empty
        cfg.WhiteBg shouldBe empty

        cfg.HiBlackBg shouldBe empty
        cfg.HiRedBg shouldBe empty
        cfg.HiGreenBg shouldBe empty
        cfg.HiYellowBg shouldBe empty
        cfg.HiBlueBg shouldBe empty
        cfg.HiMagentaBg shouldBe empty
        cfg.HiCyanBg shouldBe empty
        cfg.HiWhiteBg shouldBe empty

        cfg.Bold shouldBe empty
        cfg.Reset shouldBe empty
      }
    }

    describe("for the 8 basic colours") {
      it("by default is activated and non-bold") {
        val cfg = AnsiConsole()
        cfg.bold("-") shouldBe s"$BOLD-$RESET"
        cfg.black("-") shouldBe s"$BLACK-$RESET"
        cfg.red("-") shouldBe s"$RED-$RESET"
        cfg.green("-") shouldBe s"$GREEN-$RESET"
        cfg.yellow("-") shouldBe s"$YELLOW-$RESET"
        cfg.blue("-") shouldBe s"$BLUE-$RESET"
        cfg.magenta("-") shouldBe s"$MAGENTA-$RESET"
        cfg.cyan("-") shouldBe s"$CYAN-$RESET"
        cfg.white("-") shouldBe s"$WHITE-$RESET"
        cfg.blackBg("-") shouldBe s"$BLACK_B-$RESET"
        cfg.redBg("-") shouldBe s"$RED_B-$RESET"
        cfg.greenBg("-") shouldBe s"$GREEN_B-$RESET"
        cfg.yellowBg("-") shouldBe s"$YELLOW_B-$RESET"
        cfg.blueBg("-") shouldBe s"$BLUE_B-$RESET"
        cfg.magentaBg("-") shouldBe s"$MAGENTA_B-$RESET"
        cfg.cyanBg("-") shouldBe s"$CYAN_B-$RESET"
        cfg.whiteBg("-") shouldBe s"$WHITE_B-$RESET"
      }
      it("can be activated and bold") {
        val cfg = AnsiConsole()
        cfg.black("-", bold = true) shouldBe s"$BOLD$BLACK-$RESET"
        cfg.red("-", bold = true) shouldBe s"$BOLD$RED-$RESET"
        cfg.green("-", bold = true) shouldBe s"$BOLD$GREEN-$RESET"
        cfg.yellow("-", bold = true) shouldBe s"$BOLD$YELLOW-$RESET"
        cfg.blue("-", bold = true) shouldBe s"$BOLD$BLUE-$RESET"
        cfg.magenta("-", bold = true) shouldBe s"$BOLD$MAGENTA-$RESET"
        cfg.cyan("-", bold = true) shouldBe s"$BOLD$CYAN-$RESET"
        cfg.white("-", bold = true) shouldBe s"$BOLD$WHITE-$RESET"
        cfg.blackBg("-", bold = true) shouldBe s"$BOLD$BLACK_B-$RESET"
        cfg.redBg("-", bold = true) shouldBe s"$BOLD$RED_B-$RESET"
        cfg.greenBg("-", bold = true) shouldBe s"$BOLD$GREEN_B-$RESET"
        cfg.yellowBg("-", bold = true) shouldBe s"$BOLD$YELLOW_B-$RESET"
        cfg.blueBg("-", bold = true) shouldBe s"$BOLD$BLUE_B-$RESET"
        cfg.magentaBg("-", bold = true) shouldBe s"$BOLD$MAGENTA_B-$RESET"
        cfg.cyanBg("-", bold = true) shouldBe s"$BOLD$CYAN_B-$RESET"
        cfg.whiteBg("-", bold = true) shouldBe s"$BOLD$WHITE_B-$RESET"
      }
      it("can be activated and hi-intensity") {
        val cfg = AnsiConsole()
        cfg.black("-", hi = true) shouldBe s"\u001b[90m-$RESET"
        cfg.red("-", hi = true) shouldBe s"\u001b[91m-$RESET"
        cfg.green("-", hi = true) shouldBe s"\u001b[92m-$RESET"
        cfg.yellow("-", hi = true) shouldBe s"\u001b[93m-$RESET"
        cfg.blue("-", hi = true) shouldBe s"\u001b[94m-$RESET"
        cfg.magenta("-", hi = true) shouldBe s"\u001b[95m-$RESET"
        cfg.cyan("-", hi = true) shouldBe s"\u001b[96m-$RESET"
        cfg.white("-", hi = true) shouldBe s"\u001b[97m-$RESET"
        cfg.blackBg("-", hi = true) shouldBe s"\u001b[100m-$RESET"
        cfg.redBg("-", hi = true) shouldBe s"\u001b[101m-$RESET"
        cfg.greenBg("-", hi = true) shouldBe s"\u001b[102m-$RESET"
        cfg.yellowBg("-", hi = true) shouldBe s"\u001b[103m-$RESET"
        cfg.blueBg("-", hi = true) shouldBe s"\u001b[104m-$RESET"
        cfg.magentaBg("-", hi = true) shouldBe s"\u001b[105m-$RESET"
        cfg.cyanBg("-", hi = true) shouldBe s"\u001b[106m-$RESET"
        cfg.whiteBg("-", hi = true) shouldBe s"\u001b[107m-$RESET"
      }
      it("can turn off the reset") {
        val cfg = AnsiConsole()
        cfg.bold("-", reset = false) shouldBe s"$BOLD-"
        cfg.black("-", reset = false) shouldBe s"$BLACK-"
        cfg.red("-", reset = false) shouldBe s"$RED-"
        cfg.green("-", reset = false) shouldBe s"$GREEN-"
        cfg.yellow("-", reset = false) shouldBe s"$YELLOW-"
        cfg.blue("-", reset = false) shouldBe s"$BLUE-"
        cfg.magenta("-", reset = false) shouldBe s"$MAGENTA-"
        cfg.cyan("-", reset = false) shouldBe s"$CYAN-"
        cfg.white("-", reset = false) shouldBe s"$WHITE-"
        cfg.blackBg("-", reset = false) shouldBe s"$BLACK_B-"
        cfg.redBg("-", reset = false) shouldBe s"$RED_B-"
        cfg.greenBg("-", reset = false) shouldBe s"$GREEN_B-"
        cfg.yellowBg("-", reset = false) shouldBe s"$YELLOW_B-"
        cfg.blueBg("-", reset = false) shouldBe s"$BLUE_B-"
        cfg.magentaBg("-", reset = false) shouldBe s"$MAGENTA_B-"
        cfg.cyanBg("-", reset = false) shouldBe s"$CYAN_B-"
        cfg.whiteBg("-", reset = false) shouldBe s"$WHITE_B-"
      }
      for (bold <- Seq(false, true); reset <- Seq(false, true); hi <- Seq(false, true)) {
        it(s"can be deactivated for non-ansi use (bold: $bold, reset $reset, hi $hi)") {
          val cfg = AnsiConsole(plain = true)
          cfg.bold("-", reset = reset) shouldBe "-"
          cfg.black("-", bold = bold, reset = reset, hi = hi) shouldBe "-"
          cfg.red("-", bold = bold, reset = reset, hi = hi) shouldBe "-"
          cfg.green("-", bold = bold, reset = reset, hi = hi) shouldBe "-"
          cfg.yellow("-", bold = bold, reset = reset, hi = hi) shouldBe "-"
          cfg.blue("-", bold = bold, reset = reset, hi = hi) shouldBe "-"
          cfg.magenta("-", bold = bold, reset = reset, hi = hi) shouldBe "-"
          cfg.cyan("-", bold = bold, reset = reset, hi = hi) shouldBe "-"
          cfg.white("-", bold = bold, reset = reset, hi = hi) shouldBe "-"
          cfg.blackBg("-", bold = bold, reset = reset, hi = hi) shouldBe "-"
          cfg.redBg("-", bold = bold, reset = reset, hi = hi) shouldBe "-"
          cfg.greenBg("-", bold = bold, reset = reset, hi = hi) shouldBe "-"
          cfg.yellowBg("-", bold = bold, reset = reset, hi = hi) shouldBe "-"
          cfg.blueBg("-", bold = bold, reset = reset, hi = hi) shouldBe "-"
          cfg.magentaBg("-", bold = bold, reset = reset, hi = hi) shouldBe "-"
          cfg.cyanBg("-", bold = bold, reset = reset, hi = hi) shouldBe "-"
          cfg.whiteBg("-", bold = bold, reset = reset, hi = hi) shouldBe "-"
        }
      }
    }

    describe("for some semantic colours") {
      it("by default is activated and non-bold") {
        val cfg = AnsiConsole()
        cfg.msg("-") shouldBe s"$CYAN-$RESET"
        cfg.msg1("-") shouldBe s"$CYAN-$RESET"
        cfg.msg2("-") shouldBe s"$BLUE-$RESET"
        cfg.msg3("-") shouldBe s"$MAGENTA-$RESET"
        cfg.ok("-") shouldBe s"$GREEN-$RESET"
        cfg.warn("-") shouldBe s"$YELLOW-$RESET"
        cfg.error("-") shouldBe s"$RED-$RESET"
        cfg.comment("-") shouldBe s"\u001b[90m-$RESET"
        cfg.left("-") shouldBe s"$CYAN-$RESET"
        cfg.right("-") shouldBe s"$MAGENTA-$RESET"
        cfg.kv("-", "x") shouldBe s"$MAGENTA-$RESET: x"
      }
      it("can be activated and bold") {
        val cfg = AnsiConsole()
        cfg.msg("-", bold = true) shouldBe s"$BOLD$CYAN-$RESET"
        cfg.msg1("-", bold = true) shouldBe s"$BOLD$CYAN-$RESET"
        cfg.msg2("-", bold = true) shouldBe s"$BOLD$BLUE-$RESET"
        cfg.msg3("-", bold = true) shouldBe s"$BOLD$MAGENTA-$RESET"
        cfg.ok("-", bold = true) shouldBe s"$BOLD$GREEN-$RESET"
        cfg.warn("-", bold = true) shouldBe s"$BOLD$YELLOW-$RESET"
        cfg.error("-", bold = true) shouldBe s"$BOLD$RED-$RESET"
        cfg.comment("-", bold = true) shouldBe s"$BOLD\u001b[90m-$RESET"
        cfg.left("-", bold = true) shouldBe s"$BOLD$CYAN-$RESET"
        cfg.right("-", bold = true) shouldBe s"$BOLD$MAGENTA-$RESET"
        cfg.kv("-", "x", bold = true) shouldBe s"$BOLD$MAGENTA-$RESET: x"
      }
      it("can turn off the reset") {
        val cfg = AnsiConsole()
        cfg.msg("-", reset = false) shouldBe s"$CYAN-"
        cfg.msg1("-", reset = false) shouldBe s"$CYAN-"
        cfg.msg2("-", reset = false) shouldBe s"$BLUE-"
        cfg.msg3("-", reset = false) shouldBe s"$MAGENTA-"
        cfg.ok("-", reset = false) shouldBe s"$GREEN-"
        cfg.warn("-", reset = false) shouldBe s"$YELLOW-"
        cfg.error("-", reset = false) shouldBe s"$RED-"
        cfg.comment("-", reset = false) shouldBe s"\u001b[90m-"
        cfg.left("-", reset = false) shouldBe s"$CYAN-"
        cfg.right("-", reset = false) shouldBe s"$MAGENTA-"
        cfg.kv("-", "x", reset = false) shouldBe s"$MAGENTA-: x"
      }
      for (bold <- Seq(false, true); reset <- Seq(false, true)) {
        it(s"can be deactivated for non-ansi use (bold: $bold, reset $reset)") {
          val cfg = AnsiConsole(plain = true)
          cfg.msg("-", bold = bold, reset = reset) shouldBe s"-"
          cfg.msg1("-", bold = bold, reset = reset) shouldBe s"-"
          cfg.msg2("-", bold = bold, reset = reset) shouldBe s"-"
          cfg.msg3("-", bold = bold, reset = reset) shouldBe s"-"
          cfg.ok("-", bold = bold, reset = reset) shouldBe "-"
          cfg.warn("-", bold = bold, reset = reset) shouldBe "-"
          cfg.error("-", bold = bold, reset = reset) shouldBe "-"
          cfg.comment("-", bold = bold, reset = reset) shouldBe "-"
          cfg.left("-", bold = bold, reset = reset) shouldBe "-"
          cfg.right("-", bold = bold, reset = reset) shouldBe "-"
          cfg.kv("-", "x", bold = bold, reset = reset) shouldBe "-: x"
        }
      }
    }
  }

  describe("The verbose option") {
    val cfgNoV = AnsiConsole(plain = false, verbose = false)
    val cfgV = AnsiConsole(plain = false, verbose = true)

    it("when enabled, prints text through the vPrint and vPrintln methods") {
      withConsoleMatch {
        cfgV.vPrintln("Hey")
        cfgV.vPrint("Hey")
        cfgV.vPrint(Int.MaxValue)
        cfgV.vPrintln()
      } { case (_, out, err) =>
        err shouldBe empty
        out shouldBe "Hey\nHey2147483647\n"
      }
    }

    it("when disabled, ignores calls to vPrint and vPrintln methods") {
      withConsoleMatch {
        cfgNoV.vPrintln("Hey")
        cfgNoV.vPrint("Hey")
        cfgNoV.vPrint(Int.MinValue)
        cfgNoV.vPrintln()
      } { case (_, out, err) =>
        err shouldBe empty
        out shouldBe empty
      }
    }
  }

  describe("The text helper generator methods") {
    it("should provide a nice help header") {
      val cfg = AnsiConsole()
      cfg.helpHeader(
        "myscript.sc",
        "A script using the console.",
        "execute" -> "Does many things",
        "etc" -> "and more"
      ) shouldBe
        s"""${cfg.green("myscript.sc", bold = true)} - A script using the console.
           |
           |${cfg.cyan(" execute")} : Does many things
           |${cfg.cyan("     etc")} : and more
           |
           |${cfg.bold("Usage:")}
           |""".stripMargin
    }
  }

  describe("The ask() method") {

    def simpleAsk(
        userResponse: String,
        out: AnsiConsole = AnsiConsole()
    ): (String, Option[String]) = Streamable.closing(
      new ByteArrayInputStream(s"$userResponse\n".getBytes)
    ) { in =>
      Console.withIn(in) {
        withConsoleMatch(out.ask("Password?") { "Open sesame" }) { case (result, out, err) =>
          err shouldBe empty
          (out, result)
        }
      }
    }

    // Test yes responses
    for (response <- Seq("y", "Y", "yes", "Yah", ""))
      it(s"executes and returns Some value when the user responds $response") {
        simpleAsk(response) shouldBe ("Password? (Y/n/q): ", Some(
          "Open sesame"
        ))
      }

    // Test no responses
    for (response <- Seq("n", "N", "no", "Nope"))
      it(s"executes and returns None when the user responds $response") {
        simpleAsk(response) shouldBe ("Password? (Y/n/q): ", None)
      }

    // Test a yes responses after an invalid response
    for (response <- Seq("Xx\ny", "True\nYeah", "Maybe\n"))
      it(s"executes and returns Some value when the user responds $response") {
        simpleAsk(
          response
        ) shouldBe ("Password? (Y/n/q): Password? (Y/n/q): ", Some(
          "Open sesame"
        ))
      }

    // Test a no responses after an invalid response
    for (response <- Seq("Xx\nn", "True\nNever"))
      it(s"executes and returns Some value when the user responds $response") {
        simpleAsk(
          response
        ) shouldBe ("Password? (Y/n/q): Password? (Y/n/q): ", None)
      }

    it("Doesn't prompt the user when the yes option is set") {
      val cfg = AnsiConsole(yes = true)
      simpleAsk("anything", cfg) shouldBe ("", Some(
        "Open sesame"
      ))
    }

    it("Prints the prompts silently when verbose and yes options are set") {
      val cfg = AnsiConsole(yes = true, verbose = true)
      simpleAsk(
        "anything",
        cfg
      ) shouldBe (s"Password? (Y/n/q): ${BOLD}Y$RESET\n", Some("Open sesame"))
      val cfg2 =
        AnsiConsole(yes = true, plain = true, verbose = true)
      simpleAsk("anything", cfg2) shouldBe (s"Password? (Y/n/q): Y\n", Some("Open sesame"))
    }
  }
}
