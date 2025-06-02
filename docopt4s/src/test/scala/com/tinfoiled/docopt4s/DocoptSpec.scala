package com.tinfoiled.docopt4s

import org.scalatest.BeforeAndAfterAll
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

import scala.reflect.io.{Directory, File}

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

  val opt = Docopt(
    Map(
      "string" -> "value",
      "dir" -> Tmp.toString,
      "file" -> ExistingFile.toString,
      "nox" -> (Tmp / "nox").toString()
    )
  )

  describe("Testing the getString methods") {

    describe("when getting an optional value") {
      it("should get the present value") { opt.getStringOption("string") shouldBe Some("value") }
      it("should get a missing value") { opt.getStringOption("string") shouldBe Some("value") }
    }

    describe("when getting a required value") {
      it("should get the present value") { opt.getStringOption("no-exists") shouldBe None }
      it("should fail with a missing value") {
        val t = intercept[DocoptException] {
          opt.getString("no-exists")
        }
        t.getMessage shouldBe "Expected no-exists not found"
        t.exitCode shouldBe 1
      }
    }

    describe("when getting a required value with default") {
      it("should get the present value") { opt.getString("string", "default") shouldBe "value" }
      it("should get a missing value") { opt.getString("no-exists", "default") shouldBe "default" }
    }
  }

  describe("Other") {

    it("should get path values correctly") {
      // option
      opt.getPathOption("dir") shouldBe Some(Tmp)
      opt.getPathOption("file") shouldBe Some(ExistingFile)
      opt.getPathOption("no-exists") shouldBe None
      // direct
      opt.getPath("dir") shouldBe Tmp
      opt.getPath("file") shouldBe ExistingFile
      val t = intercept[DocoptException] { opt.getPath("no-exists") }
      t.getMessage shouldBe "Expected no-exists not found"
      t.exitCode shouldBe 1
      // default (TODO)
    }

    it("should get directory values correctly") {
      // option
      opt.getDirectoryOption("dir") shouldBe Some(Tmp)
      opt.getDirectoryOption("no-exists") shouldBe None
      // direct
      opt.getDirectory("dir") shouldBe Tmp
      // When the directory doesn't exist in the list of arguments
      val t = intercept[DocoptException] { opt.getDirectory("no-exists") }
      t.getMessage shouldBe "Expected no-exists not found"
      t.exitCode shouldBe 1
      // When the directory doesn't exist in the filesystem
      val t2 = intercept[DocoptException] { opt.getDirectory("nox") }
      t2.getMessage shouldBe s"Directory doesn't exist: $Tmp/nox"
      t2.exitCode shouldBe 1
      // default (TODO)
    }

    it("should get file values correctly") {
      // option
      opt.getFileOption("file") shouldBe Some(ExistingFile)
      opt.getFileOption("no-exists") shouldBe None
      // direct
      opt.getFile("file") shouldBe ExistingFile
      val t = intercept[DocoptException] { opt.getFile("no-exists") }
      t.getMessage shouldBe "Expected no-exists not found"
      t.exitCode shouldBe 1
      // default (TODO)
    }

  }
}
