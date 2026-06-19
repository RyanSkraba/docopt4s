package com.tinfoiled.docopt4s.testkit.example

import com.tinfoiled.docopt4s.testkit.MultiTaskMainSpec

import java.io.ByteArrayInputStream
import java.nio.charset.StandardCharsets
import scala.util.Using

/** Unit tests for [[EchoTaskSpec]] */
class EchoTaskSpec extends MultiTaskMainSpec(ExampleGo, Some(EchoTask)) {

  describe(s"$MainName $TaskCmd basic scenario") {
    it("should echo the input with extra details") {
      Using.resource(
        new ByteArrayInputStream("""One
            |Two
            |Three
            |
            |Four""".stripMargin.getBytes(StandardCharsets.UTF_8))
      ) { in =>
        Console.withIn(in) {
          withGoStdout(TaskCmd)
        } shouldBe
          """Reading from stdin (Ctrl+D to finish):
            |One
            |Two
            |Three
            |
            |Four
            |Finished reading from stdin after 5 lines
            |""".stripMargin
      }
    }

    it("with no input, it should not echo any lines.") {
      Using.resource(new ByteArrayInputStream(Array.empty)) { in =>
        Console.withIn(in) {
          withGoStdout(TaskCmd)
        } shouldBe
          """Finished reading from stdin after 0 lines
            |""".stripMargin
      }
    }
  }
}
