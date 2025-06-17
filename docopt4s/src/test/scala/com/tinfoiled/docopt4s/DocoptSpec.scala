package com.tinfoiled.docopt4s

import org.scalactic.source
import org.scalatest.{Assertion, BeforeAndAfterAll}
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

import scala.reflect.ClassTag
import scala.reflect.io.{Directory, File, Path}

/** Test the [[Docopt]] class.
  *
  * Note that the [[Docopt.apply()]] that parses documentation strings is thoroughly tested in the testkit project. This
  * class focuses on the fetchers.
  */
class DocoptSpec extends AnyFunSpecLike with BeforeAndAfterAll with Matchers {

  /** A local temporary directory for test file storage. */
  val Tmp: Directory = Directory.makeTemp(getClass.getSimpleName)

  /** The directory that we're being run in (used for relative paths) */
  val Pwd: Path = Directory(".").toCanonical

  /** A file with a basic scenario. */
  val ExistingFile: File = (Tmp / "file.txt").createFile()
  ExistingFile.writeAll("file")

  /** Delete temporary resources after the script. */
  override protected def afterAll(): Unit =
    try { Tmp.deleteRecursively() }
    catch { case ex: Exception => ex.printStackTrace() }

  /** Shortcut to generate a Docopt */
  def optWith(elems: (String, Any)*): Docopt = Docopt(Map.from(elems))

  /** A standard opts */
  val opt: Docopt = optWith(
    "string" -> "value",
    "strings" -> Seq("x", "y"),
    "int" -> 12345,
    "bool" -> true,
    "dir" -> Tmp.toString,
    "file" -> ExistingFile.toString,
    "nox" -> (Tmp / "nox").toString()
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

    describe("when converting other types") {
      it("should convert a string list") { opt.getBoolean("strings", default = false) shouldBe true }
      it(s"should convert a false (empty) string list") {
        optWith("x" -> Seq.empty).getBoolean("x", default = true) shouldBe false
      }
      it("should convert a string") { opt.getBoolean("string", default = false) shouldBe false }
      for (x <- Seq("true", "TRUE", "True")) {
        it(s"should convert a true string: $x") { optWith("x" -> x).getBoolean("x", default = false) shouldBe true }
        it(s"should convert a true (non-empty) string list: $x") {
          optWith("x" -> Seq(x)).getBoolean("x", default = false) shouldBe true
        }
      }
      for (x <- Seq("", "false", "1", "Anything1", "Yes")) {
        it(s"should convert a false string: $x") { optWith("x" -> x).getBoolean("x", default = true) shouldBe false }
        it(s"should convert a true (non-empty) string list: $x") {
          optWith("x" -> Seq(x)).getBoolean("x", default = false) shouldBe true
        }
      }
      it("should convert an int") { opt.getBoolean("int", default = false) shouldBe false }
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

    describe("when converting other types") {
      it("should fail to convert a string") {
        failOn(opt.getInt("string", -99999)) shouldBe "Expected an integer for string, but got value"
      }
      it("should convert a string") { optWith("x" -> 98765).getInt("x", -99999) shouldBe 98765 }
      it("should fail to convert a string list") {
        failOn(opt.getInt("strings", -99999)) shouldBe "Expected an integer for strings, but got x,y"
      }
      it("should fail to convert a boolean") {
        failOn(opt.getInt("bool", -99999)) shouldBe "Expected an integer for bool, but got true"
      }
    }
  }

  describe("Testing the getPath methods") {
    describe("when getting an optional value") {
      it("should get when present") { opt.getPathOption("dir") shouldBe Some(Tmp) }
      it("should get when missing") { opt.getPathOption("missing") shouldBe None }
    }

    describe("when getting a required value") {
      it("should get when present") { opt.getPath("dir") shouldBe Tmp }
      it("should fail when missing") { failOnMissing() { opt.getPath("missing") } }
    }

    describe("when getting a required value with default") {
      it("should get when present") { opt.getPathOr("dir", Tmp / "x") shouldBe Tmp }
      it("should get when missing") { opt.getPathOr("missing", Tmp / "x") shouldBe (Tmp / "x") }
    }

    describe("when converting other types") {
      it("should fail to convert a string") {
        assume(!(Pwd / "value").exists)
        failOn(opt.getPathOr("string", Tmp)) shouldBe s"Path doesn't exist: $Pwd/value"
      }
      it("should fail to convert a string list") {
        assume(!(Pwd / "x,y").exists)
        failOn(opt.getPathOr("strings", Tmp)) shouldBe s"Path doesn't exist: $Pwd/x,y"
      }
      it("should fail to convert a boolean") {
        assume(!(Pwd / "true").exists)
        failOn(opt.getPathOr("bool", Tmp)) shouldBe s"Path doesn't exist: $Pwd/true"
      }
      it("should fail to convert a int") {
        assume(!(Pwd / "12345").exists)
        failOn(opt.getPathOr("int", Tmp)) shouldBe s"Path doesn't exist: $Pwd/12345"
      }
    }
  }

  describe("Testing the getFile methods") {

    val default = (Tmp / "x").toFile

    describe("when getting an optional value") {
      it("should get when present") { opt.getFileOption("file") shouldBe Some(ExistingFile) }
      it("should get when missing") { opt.getFileOption("missing") shouldBe None }
    }

    describe("when getting a required value") {
      it("should get when present") { opt.getFile("file") shouldBe ExistingFile }
      it("should fail when missing") { failOnMissing() { opt.getFile("missing") } }
    }

    describe("when getting a required value with default") {
      it("should get when present") { opt.getFileOr("file", default) shouldBe ExistingFile }
      it("should get when missing") { opt.getFileOr("missing", default) shouldBe (Tmp / "x") }
    }

    describe("when converting other types") {
      it("should fail to convert a string") {
        assume(!(Pwd / "value").exists)
        failOn(opt.getFileOr("string", default)) shouldBe s"File doesn't exist: $Pwd/value"
      }
      it("should fail to convert a string list") {
        assume(!(Pwd / "x,y").exists)
        failOn(opt.getFileOr("strings", default)) shouldBe s"File doesn't exist: $Pwd/x,y"
      }
      it("should fail to convert a boolean") {
        assume(!(Pwd / "true").exists)
        failOn(opt.getFileOr("bool", default)) shouldBe s"File doesn't exist: $Pwd/true"
      }
      it("should fail to convert a int") {
        assume(!(Pwd / "12345").exists)
        failOn(opt.getFileOr("int", default)) shouldBe s"File doesn't exist: $Pwd/12345"
      }
    }
  }

  describe("Testing the getDirectory methods") {

    val default = (Tmp / "x").toDirectory

    describe("when getting an optional value") {
      it("should get when present") { opt.getDirectoryOption("dir") shouldBe Some(Tmp) }
      it("should get when missing") { opt.getDirectoryOption("missing") shouldBe None }
    }

    describe("when getting a required value") {
      it("should get when present") { opt.getDirectory("dir") shouldBe Tmp }
      it("should fail when missing") { failOnMissing() { opt.getDirectory("missing") } }
    }

    describe("when getting a required value with default") {
      it("should get when present") { opt.getDirectoryOr("dir", default) shouldBe Tmp }
      it("should get when missing") { opt.getDirectoryOr("missing", default) shouldBe (Tmp / "x") }
    }

    describe("when converting other types") {
      it("should fail to convert a string") {
        assume(!(Pwd / "value").exists)
        failOn(opt.getDirectoryOr("string", default)) shouldBe s"Directory doesn't exist: $Pwd/value"
      }
      it("should fail to convert a string list") {
        assume(!(Pwd / "x,y").exists)
        failOn(opt.getDirectoryOr("strings", default)) shouldBe s"Directory doesn't exist: $Pwd/x,y"
      }
      it("should fail to convert a boolean") {
        assume(!(Pwd / "true").exists)
        failOn(opt.getDirectoryOr("bool", default)) shouldBe s"Directory doesn't exist: $Pwd/true"
      }
      it("should fail to convert a int") {
        assume(!(Pwd / "12345").exists)
        failOn(opt.getDirectoryOr("int", default)) shouldBe s"Directory doesn't exist: $Pwd/12345"
      }
    }
  }
}
