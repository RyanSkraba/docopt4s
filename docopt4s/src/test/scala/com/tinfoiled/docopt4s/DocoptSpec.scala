package com.tinfoiled.docopt4s

import com.tinfoiled.docopt4s.AnsiConsole.withConsoleMatch
import org.scalatest.BeforeAndAfterAll
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

import java.io.ByteArrayInputStream
import scala.io.AnsiColor._
import scala.reflect.io.Streamable

/** Test the [[Docopt]] class.
  *
  * Note that the [[Docopt.apply()]] that parses documentation strings is thoroughly tested in the testkit project. This
  * class focuses on the fetchers.
  */
class DocoptSpec extends AnyFunSpecLike with BeforeAndAfterAll with Matchers {

  describe("Testing the key -> value fetch methods") {

    val opts = Docopt(Map("key" -> "value"))

    it("should get string values correctly") {
      opts.getStringOption("key") shouldBe Some("value")
      opts.getStringOption("no-exists") shouldBe None
      opts.getString("key") shouldBe "value"
      val t = intercept[DocoptException] { opts.getString("no-exists") }
      t.getMessage shouldBe "Expected no-exists not found"
      t.exitCode shouldBe 1
      opts.getString("key", "default") shouldBe "value"
      opts.getString("no-exists", "default") shouldBe "default"
    }
  }
}
