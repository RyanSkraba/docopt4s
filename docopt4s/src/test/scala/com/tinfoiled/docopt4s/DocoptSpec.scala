package com.tinfoiled.docopt4s

import org.scalactic.source
import org.scalatest.{Assertion, BeforeAndAfterAll}
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

import scala.reflect.ClassTag
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

  val opt: Docopt = Docopt(
    Map(
      "string" -> "value",
      "strings" -> Seq("x", "y"),
      "int" -> 12345,
      "bool" -> true,
      "dir" -> Tmp.toString,
      "file" -> ExistingFile.toString,
      "nox" -> (Tmp / "nox").toString()
    )
  )

  /** Helper method to capture a DocoptException with no docopt and an exitCode of 1.
    *
    * @param thunk
    *   Code to execute that should throw an exception.
    * @return
    *   the message in the DocoptException
    */
  def failOn(thunk: => Any)(implicit classTag: ClassTag[DocoptException], pos: source.Position): String = {
    val t = intercept[DocoptException] { thunk }(classTag, pos)
    Option(t.docopt) shouldBe None
    t.exitCode shouldBe 1
    t.getMessage
  }

  /** Helper method to capture a DocoptException with no docopt and an exitCode of 1.
    *
    * @param thunk
    *   Code to execute that should throw an exception.
    * @return
    *   the message in the DocoptException
    */
  def failOnMissing(key: String = "missing")(thunk: => Any): Assertion =
    failOn { thunk } shouldBe s"Expected $key not found"

  describe("Testing the getString methods") {
    describe("when getting an optional value") {
      it("should get when present") { opt.getStringOption("string") shouldBe Some("value") }
      it("should get when missing") { opt.getStringOption("missing") shouldBe None }
    }

    describe("when getting a required value") {
      it("should get when present") { opt.getString("string") shouldBe "value" }
      it("should fail when missing") { failOnMissing() { opt.getString("missing") } }
    }

    describe("when getting a required value with default") {
      it("should get when present") { opt.getString("string", "default") shouldBe "value" }
      it("should get when missing") { opt.getString("missing", "default") shouldBe "default" }
    }

    describe("when converting other types") {
      it("should convert a string list") { opt.getString("strings", "default") shouldBe "x,y" }
      it("should convert an int") { opt.getString("int", "default") shouldBe "12345" }
      it("should convert a boolean") { opt.getString("bool", "default") shouldBe "true" }
    }
  }

  describe("Testing the getStrings methods") {
    describe("when getting an optional value") {
      it("should get when present") { opt.getStringsOption("strings") shouldBe Some(Seq("x", "y")) }
      it("should get when missing") { opt.getStringsOption("missing") shouldBe None }
    }

    describe("when getting a required value") {
      it("should get when present") { opt.getStrings("strings") shouldBe Seq("x", "y") }
      it("should fail when missing") { failOnMissing() { opt.getStrings("missing") } }
    }

    describe("when getting a required value with default") {
      it("should get when present") { opt.getStrings("strings", Seq("def")) shouldBe Seq("x", "y") }
      it("should get when missing") { opt.getStrings("missing", Seq("def")) shouldBe Seq("def") }
    }

    describe("when converting other types") {
      it("should convert a string") { opt.getStrings("string", Seq.empty) shouldBe Seq("value") }
      it("should convert an int") { opt.getStrings("int", Seq.empty) shouldBe Seq("12345") }
      it("should convert a boolean") { opt.getStrings("bool", Seq.empty) shouldBe Seq("true") }
    }
  }

  describe("Testing the getBoolean methods") {
    describe("when getting an optional value") {
      it("should get when present") { opt.getBooleanOption("bool") shouldBe Some(true) }
      it("should get when missing") { opt.getBooleanOption("missing") shouldBe None }
    }

    describe("when getting a required value") {
      it("should get when present") { opt.getBoolean("bool") shouldBe true }
      it("should fail when missing") { failOnMissing() { opt.getBoolean("missing") } }
    }

    describe("when getting a required value with default") {
      it("should get when present") { opt.getBoolean("bool", default = false) shouldBe true }
      it("should get when missing") { opt.getBoolean("missing", default = false) shouldBe false }
    }
  }

  describe("Testing the getInt methods") {
    describe("when getting an optional value") {
      it("should get when present") { opt.getIntOption("int") shouldBe Some(12345) }
      it("should get when missing") { opt.getIntOption("missing") shouldBe None }
    }

    describe("when getting a required value") {
      it("should get when present") { opt.getInt("int") shouldBe 12345 }
      it("should fail when missing") { failOnMissing() { opt.getInt("missing") } }
    }

    describe("when getting a required value with default") {
      it("should get when present") { opt.getInt("int", -99999) shouldBe 12345 }
      it("should get when missing") { opt.getInt("missing", -99999) shouldBe -99999 }
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
