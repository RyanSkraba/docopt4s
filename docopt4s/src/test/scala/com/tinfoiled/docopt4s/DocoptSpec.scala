package com.tinfoiled.docopt4s

import com.tinfoiled.docopt4s.AnsiConsole.withConsoleMatch
import org.scalatest.BeforeAndAfterAll
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

import java.io.ByteArrayInputStream
import scala.io.AnsiColor._
import scala.reflect.io.{Directory, File, Path, Streamable}

/** Test the [[Docopt]] class.
  *
  * Note that the [[Docopt.apply()]] that parses documentation strings is thoroughly tested in the testkit project. This
  * class focuses on the fetchers.
  */
class DocoptSpec extends AnyFunSpecLike with BeforeAndAfterAll with Matchers {

  /** A local temporary directory for test file storage. */
  val Tmp: Directory = Directory.makeTemp(getClass.getSimpleName)

  /** A file with a basic scenario. */
  val ExistingFile: File = (Tmp / "file.txt").createFile()
  ExistingFile.writeAll("file")

  /** Delete temporary resources after the script. */
  override protected def afterAll(): Unit =
    try { Tmp.deleteRecursively() }
    catch { case ex: Exception => ex.printStackTrace() }

  describe("Testing the key -> value fetch methods") {

    val opts = Docopt(Map("string" -> "value", "dir" -> Tmp.toString, "file" -> ExistingFile.toString))

    it("should get string values correctly") {
      // option
      opts.getStringOption("string") shouldBe Some("value")
      opts.getStringOption("no-exists") shouldBe None
      // direct
      opts.getString("string") shouldBe "value"
      val t = intercept[DocoptException] { opts.getString("no-exists") }
      t.getMessage shouldBe "Expected no-exists not found"
      t.exitCode shouldBe 1
      // default
      opts.getString("string", "default") shouldBe "value"
      opts.getString("no-exists", "default") shouldBe "default"
    }

    it("should get path values correctly") {
      // option
      opts.getPathOption("dir") shouldBe Some(Tmp)
      opts.getPathOption("file") shouldBe Some(ExistingFile)
      opts.getPathOption("no-exists") shouldBe None
      // direct
      opts.getPath("dir") shouldBe Tmp
      opts.getPath("file") shouldBe ExistingFile
      val t = intercept[DocoptException] { opts.getPath("no-exists") }
      t.getMessage shouldBe "Expected no-exists not found"
      t.exitCode shouldBe 1
      // default (TODO)
    }

    it("should get directory values correctly") {
      // option
      opts.getDirectoryOption("dir") shouldBe Some(Tmp)
      opts.getDirectoryOption("no-exists") shouldBe None
      // direct
      opts.getDirectory("dir") shouldBe Tmp
      val t = intercept[DocoptException] { opts.getDirectory("no-exists") }
      t.getMessage shouldBe "Expected no-exists not found"
      t.exitCode shouldBe 1
      // default (TODO)
    }

    it("should get file values correctly") {
      // option
      opts.getFileOption("file") shouldBe Some(ExistingFile)
      opts.getFileOption("no-exists") shouldBe None
      // direct
      opts.getFile("file") shouldBe ExistingFile
      val t = intercept[DocoptException] { opts.getFile("no-exists") }
      t.getMessage shouldBe "Expected no-exists not found"
      t.exitCode shouldBe 1
      // default (TODO)
    }

  }
}
