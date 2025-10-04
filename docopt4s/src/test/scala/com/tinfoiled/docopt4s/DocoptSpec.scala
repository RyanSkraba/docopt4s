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

  /** A local temporary directory for test file storage.  Note that we can't use TmpDir from docopt4s-testkit yet. */
  val Tmp: Directory = Directory.makeTemp(getClass.getSimpleName)

  /** The directory that we're being run in (used for relative paths) */
  val Pwd: Path = Directory(".").toCanonical

  /** A file with a basic scenario. */
  val ExistingFile: File = (Tmp / "file.txt").createFile()
  ExistingFile.writeAll("file")

  val NonExistingPath: Path = Tmp / "nox"

  val DfltDir: Directory = (Tmp / "dflt").toDirectory

  val DfltFile: File = DfltDir.toFile

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
    "nox" -> NonExistingPath.toString,
    "badFileParent" -> (ExistingFile / "cant-exist").toString
  )

  /** A path validator with the tag source */
  val vldTag: PathValidator = PathValidator().withTag("Source")

  /** A path validator that ensures a file doesn't exist */
  val vldNox: PathValidator = PathValidator().doesntExist()

  /** A path validator that doesn't care if a file exists or not */
  val vldMaybe: PathValidator = PathValidator().optionallyExists()

  /** A path validator that has a root */
  val vldRoot: PathValidator = PathValidator().withRoot(Tmp.toString())

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
      it("should get when present") { opt.string.getOption("string") shouldBe Some("value") }
      it("should get when missing") { opt.string.getOption("missing") shouldBe None }
    }

    describe("when getting a required value") {
      it("should get when present") { opt.string.get("string") shouldBe "value" }
      it("should fail when missing") { failOnMissing() { opt.string.get("missing") } }
    }

    describe("when getting a required value with default") {
      it("should get when present") { opt.string.getOr("string", "default") shouldBe "value" }
      it("should get when missing") { opt.string.getOr("missing", "default") shouldBe "default" }
    }

    describe("when converting other types") {
      it("should convert a string list") { opt.string.getOr("strings", "default") shouldBe "x,y" }
      it("should convert an int") { opt.string.getOr("int", "default") shouldBe "12345" }
      it("should convert a boolean") { opt.string.getOr("bool", "default") shouldBe "true" }
    }
  }

  describe("Testing the getStrings methods") {
    describe("when getting an optional value") {
      it("should get when present") { opt.strings.getOption("strings") shouldBe Some(Seq("x", "y")) }
      it("should get when missing") { opt.strings.getOption("missing") shouldBe None }
    }

    describe("when getting a required value") {
      it("should get when present") { opt.strings.get("strings") shouldBe Seq("x", "y") }
      it("should fail when missing") { failOnMissing() { opt.strings.get("missing") } }
    }

    describe("when getting a required value with default") {
      it("should get when present") { opt.strings.getOr("strings", Seq("def")) shouldBe Seq("x", "y") }
      it("should get when missing") { opt.strings.getOr("missing", Seq("def")) shouldBe Seq("def") }
    }

    describe("when converting other types") {
      it("should convert a string") { opt.strings.getOr("string", Seq.empty) shouldBe Seq("value") }
      it("should convert an int") { opt.strings.getOr("int", Seq.empty) shouldBe Seq("12345") }
      it("should convert a boolean") { opt.strings.getOr("bool", Seq.empty) shouldBe Seq("true") }
    }
  }

  describe("Testing the getBoolean methods") {
    describe("when getting an optional value") {
      it("should get when present") { opt.boolean.getOption("bool") shouldBe Some(true) }
      it("should get when missing") { opt.boolean.getOption("missing") shouldBe None }
    }

    describe("when getting a required value") {
      it("should get when present") { opt.boolean.get("bool") shouldBe true }
      it("should fail when missing") { failOnMissing() { opt.boolean.get("missing") } }
    }

    describe("when getting a required value with default") {
      it("should get when present") { opt.boolean.getOr("bool", default = false) shouldBe true }
      it("should get when missing") { opt.boolean.getOr("missing", default = false) shouldBe false }
    }

    describe("when using the flag shortcut") {
      it("should get when present") { opt.flag("bool") shouldBe true }
      it("should get when missing") { opt.flag("missing") shouldBe false }
    }

    describe("when converting other types") {
      it("should convert a string list") { opt.boolean.getOr("strings", default = true) shouldBe false }
      it(s"should convert a false (empty) string list") {
        optWith("x" -> Seq.empty).boolean.getOr("x", default = true) shouldBe false
      }
      it("should convert a string") { opt.boolean.getOr("string", default = false) shouldBe false }
      for (x <- Seq(true, "true", "TRUE", "True")) {
        describe(s"when converting ${x.getClass}:'$x'") {
          it(s"should convert true values") {
            optWith("x" -> x).boolean.get("x") shouldBe true
            optWith("x" -> x).boolean.getOr("x", default = false) shouldBe true
          }
          it(s"should be the same as the flag shortcut") {
            optWith("x" -> x).flag("x") shouldBe true
          }
          it(s"any single element string list resolves as true") {
            optWith("x" -> Seq(x)).boolean.getOr("x", default = false) shouldBe true
          }
          it(s"any 2+ element string list resolves as false") {
            optWith("x" -> Seq(x, "true")).boolean.getOr("x", default = true) shouldBe false
          }
        }
      }
      for (x <- Seq(false, "false", "tru", "yes", 1, 0, Seq.empty, "1", "Anything1", "Yes", "")) {
        describe(s"when converting ${x.getClass}:'$x'") {
          it(s"should convert false values") {
            optWith("x" -> x).boolean.get("x") shouldBe false
            optWith("x" -> x).boolean.getOr("x", default = true) shouldBe false
          }
          it(s"should be the same as the flag shortcut") {
            optWith("x" -> x).flag("x") shouldBe false
          }
          it(s"any single string list resolves as false") {
            optWith("x" -> Seq(x, x)).boolean.getOr("x", default = true) shouldBe false
          }
        }
      }
    }
  }

  describe("Testing the getInt methods") {
    describe("when getting an optional value") {
      it("should get when present") { opt.int.getOption("int") shouldBe Some(12345) }
      it("should get when missing") { opt.int.getOption("missing") shouldBe None }
    }

    describe("when getting a required value") {
      it("should get when present") { opt.int.get("int") shouldBe 12345 }
      it("should fail when missing") { failOnMissing() { opt.int.get("missing") } }
    }

    describe("when getting a required value with default") {
      it("should get when present") { opt.int.getOr("int", -99999) shouldBe 12345 }
      it("should get when missing") { opt.int.getOr("missing", -99999) shouldBe -99999 }
    }

    describe("when converting other types") {
      it("should fail to convert a string") {
        failOn(opt.int.getOr("string", -99999)) shouldBe "Expected an integer for string, but got value"
      }
      it("should convert a string") { optWith("x" -> 98765).int.getOr("x", -99999) shouldBe 98765 }
      it("should fail to convert a string list") {
        failOn(opt.int.getOr("strings", -99999)) shouldBe "Expected an integer for strings, but got x,y"
      }
      it("should fail to convert a boolean") {
        failOn(opt.int.getOr("bool", -99999)) shouldBe "Expected an integer for bool, but got true"
      }
    }
  }

  describe("Testing the getPath methods") {

    describe("when getting an optional value") {
      it("should get when present") { opt.path.getOption("dir") shouldBe Some(Tmp) }
      it("should get when missing") { opt.path.getOption("missing") shouldBe None }
    }

    describe("when getting a required value") {
      it("should get when present as dir") { opt.path.get("dir") shouldBe Tmp }
      it("should get when present as file") { opt.path.get("file") shouldBe ExistingFile }
      it("should fail when missing") { failOnMissing() { opt.path.get("missing") } }
    }

    describe("when getting a required value with default") {
      it("should get when present") { opt.path.getOr("dir", DfltDir) shouldBe Tmp }
      it("should get when missing") { opt.path.getOr("missing", DfltDir) shouldBe DfltDir }
    }

    // The same tests as above using a specified root, with relative and absolute option values
    for (tc <- Map("absolute" -> opt, "relative" -> optWith("dir" -> ".", "file" -> ExistingFile.name))) {
      describe(s"Testing the getPath methods with a specified root and ${tc._1} argument") {
        describe("when getting an optional value") {
          it("should get when present") { tc._2.path.getOption("dir", vldRoot) shouldBe Some(Tmp) }
          it("should get when missing") { tc._2.path.getOption("missing", vldRoot) shouldBe None }
        }

        describe("when getting a required value") {
          it("should get when present as dir") { tc._2.path.get("dir", vldRoot) shouldBe Tmp }
          it("should get when present as file") { tc._2.path.get("file", vldRoot) shouldBe ExistingFile }
          it("should fail when missing") { failOnMissing() { tc._2.path.get("missing", vldRoot) } }
        }

        describe("when getting a required value with default") {
          it("should get when present") { tc._2.path.getOr("dir", DfltDir, vldRoot) shouldBe Tmp }
          it("should get when missing") { tc._2.path.getOr("missing", DfltDir, vldRoot) shouldBe (DfltDir) }
        }
      }
    }

    describe("when requiring that it doesn't exist") {
      it("should get when it doesn't exist") { opt.path.get("nox", vldNox) shouldBe NonExistingPath }
      it("should fail when it does exist as a file") {
        failOn(opt.path.get("file", vldNox)) shouldBe s"Path already exists: $ExistingFile"
        failOn(opt.path.get("file", vldNox.withTag("Src"))) shouldBe s"Src already exists: $ExistingFile"
      }
      it("should fail when it does exist as a directory") {
        failOn(opt.path.get("dir", vldNox)) shouldBe s"Path already exists: $Tmp"
        failOn(opt.path.get("dir", vldNox.withTag("Src"))) shouldBe s"Src already exists: $Tmp"
      }
      it("should fail when it doesn't exist but one of the parent segments is a file") {
        failOn(
          opt.path.get("badFileParent", vldNox)
        ) shouldBe s"Path is uncreatable, $ExistingFile exists: $ExistingFile/cant-exist"
        failOn(
          opt.path.get("badFileParent", vldNox.withTag("Src"))
        ) shouldBe s"Src is uncreatable, $ExistingFile exists: $ExistingFile/cant-exist"
      }
    }

    describe("when optionally it exists") {
      it("should get when it doesn't exist") { opt.path.get("nox", vldMaybe) shouldBe NonExistingPath }
      it("should get when it exists as a file") { opt.path.get("file", vldMaybe) shouldBe ExistingFile }
      it("should get when it exists as a directory") { opt.path.get("dir", vldMaybe) shouldBe Tmp }
    }

    describe("when converting other types") {
      it("should fail to convert a string") {
        assume(!(Pwd / "value").exists)
        failOn(opt.path.getOr("string", Tmp)) shouldBe s"Path doesn't exist: $Pwd/value"
        failOn(opt.path.getOr("string", Tmp, vldTag)) shouldBe s"Source doesn't exist: $Pwd/value"
      }
      it("should fail to convert a string list") {
        assume(!(Pwd / "x,y").exists)
        failOn(opt.path.getOr("strings", Tmp)) shouldBe s"Path doesn't exist: $Pwd/x,y"
        failOn(opt.path.getOr("strings", Tmp, vldTag)) shouldBe s"Source doesn't exist: $Pwd/x,y"
      }
      it("should fail to convert a boolean") {
        assume(!(Pwd / "true").exists)
        failOn(opt.path.getOr("bool", Tmp)) shouldBe s"Path doesn't exist: $Pwd/true"
        failOn(opt.path.getOr("bool", Tmp, vldTag)) shouldBe s"Source doesn't exist: $Pwd/true"
      }
      it("should fail to convert a int") {
        assume(!(Pwd / "12345").exists)
        failOn(opt.path.getOr("int", Tmp)) shouldBe s"Path doesn't exist: $Pwd/12345"
        failOn(opt.path.getOr("int", Tmp, vldTag)) shouldBe s"Source doesn't exist: $Pwd/12345"
      }
    }
  }

  describe("Testing the getFile methods") {

    describe("when getting an optional value") {
      it("should get when present") { opt.file.getOption("file") shouldBe Some(ExistingFile) }
      it("should get when missing") { opt.file.getOption("missing") shouldBe None }
    }

    describe("when getting a required value") {
      it("should get when present") { opt.file.get("file") shouldBe ExistingFile }
      it("should fail when present but the wrong type") {
        failOn(opt.file.get("dir")) shouldBe s"Expected a file, found directory: $Tmp"
        failOn(opt.file.get("dir", vldTag)) shouldBe s"Source expected a file, found directory: $Tmp"
      }
      it("should fail when missing") { failOnMissing() { opt.file.get("missing") } }
    }

    describe("when getting a required value with default") {
      it("should get when present") { opt.file.getOr("file", DfltFile) shouldBe ExistingFile }
      it("should get when missing") { opt.file.getOr("missing", DfltFile) shouldBe DfltFile }
    }

    describe("when requiring that it doesn't exist") {
      it("should get when it doesn't exist") { opt.file.get("nox", vldNox) shouldBe NonExistingPath }
      it("should fail when it does exist as a file") {
        failOn(opt.file.get("file", vldNox)) shouldBe s"File already exists: $ExistingFile"
        failOn(opt.file.get("file", vldNox.withTag("Src"))) shouldBe s"Src already exists: $ExistingFile"
      }
      it("should fail when it does exist as a directory") {
        failOn(opt.file.get("dir", vldNox)) shouldBe s"File already exists: $Tmp"
        failOn(opt.file.get("dir", vldNox.withTag("Src"))) shouldBe s"Src already exists: $Tmp"
      }
      it("should fail when it doesn't exist but one of the parent segments is a file") {
        failOn(
          opt.file.get("badFileParent", vldNox)
        ) shouldBe s"File is uncreatable, $ExistingFile exists: $ExistingFile/cant-exist"
        failOn(
          opt.file.get("badFileParent", vldNox.withTag("Src"))
        ) shouldBe s"Src is uncreatable, $ExistingFile exists: $ExistingFile/cant-exist"
      }
    }

    describe("when optionally it exists") {
      it("should get when it doesn't exist") { opt.file.get("nox", vldMaybe) shouldBe NonExistingPath }
      it("should get when it exists as a file") { opt.file.get("file", vldMaybe) shouldBe ExistingFile }
      it("should fail when present but the wrong type") {
        failOn(opt.file.get("dir", vldMaybe)) shouldBe s"Expected a file, found directory: $Tmp"
        failOn(opt.file.get("dir", vldMaybe.withTag("Src"))) shouldBe s"Src expected a file, found directory: $Tmp"
      }
    }

    describe("when converting other types") {
      it("should fail to convert a string") {
        assume(!(Pwd / "value").exists)
        failOn(opt.file.getOr("string", DfltFile)) shouldBe s"File doesn't exist: $Pwd/value"
        failOn(opt.file.getOr("string", DfltFile, vldTag)) shouldBe s"Source doesn't exist: $Pwd/value"
      }
      it("should fail to convert a string list") {
        assume(!(Pwd / "x,y").exists)
        failOn(opt.file.getOr("strings", DfltFile)) shouldBe s"File doesn't exist: $Pwd/x,y"
        failOn(opt.file.getOr("strings", DfltFile, vldTag)) shouldBe s"Source doesn't exist: $Pwd/x,y"
      }
      it("should fail to convert a boolean") {
        assume(!(Pwd / "true").exists)
        failOn(opt.file.getOr("bool", DfltFile)) shouldBe s"File doesn't exist: $Pwd/true"
        failOn(opt.file.getOr("bool", DfltFile, vldTag)) shouldBe s"Source doesn't exist: $Pwd/true"
      }
      it("should fail to convert a int") {
        assume(!(Pwd / "12345").exists)
        failOn(opt.file.getOr("int", DfltFile)) shouldBe s"File doesn't exist: $Pwd/12345"
        failOn(opt.file.getOr("int", DfltFile, vldTag)) shouldBe s"Source doesn't exist: $Pwd/12345"
      }
    }
  }

  describe("Testing the getDirectory methods") {

    describe("when getting an optional value") {
      it("should get when present") { opt.dir.getOption("dir") shouldBe Some(Tmp) }
      it("should get when missing") { opt.dir.getOption("missing") shouldBe None }
    }

    describe("when getting a required value") {
      it("should get when present") { opt.dir.get("dir") shouldBe Tmp }
      it("should fail when present but the wrong type") {
        failOn(opt.dir.get("file")) shouldBe s"Expected a directory, found file: $ExistingFile"
        failOn(opt.dir.get("file", vldTag)) shouldBe s"Source expected a directory, found file: $ExistingFile"
      }
      it("should fail when missing") { failOnMissing() { opt.dir.get("missing") } }
    }

    describe("when getting a required value with default") {
      it("should get when present") { opt.dir.getOr("dir", DfltDir) shouldBe Tmp }
      it("should get when missing") { opt.dir.getOr("missing", DfltDir) shouldBe DfltDir }
    }

    describe("when requiring that it doesn't exist") {
      it("should get when it doesn't exist") { opt.dir.get("nox", vldNox) shouldBe NonExistingPath }
      it("should fail when it does exist as a file") {
        failOn(opt.dir.get("file", vldNox)) shouldBe s"Directory already exists: $ExistingFile"
        failOn(opt.dir.get("file", vldNox.withTag("Src"))) shouldBe s"Src already exists: $ExistingFile"
      }
      it("should fail when it does exist as a directory") {
        failOn(opt.dir.get("dir", vldNox)) shouldBe s"Directory already exists: $Tmp"
        failOn(opt.dir.get("dir", vldNox.withTag("Src"))) shouldBe s"Src already exists: $Tmp"
      }
      it("should fail when it doesn't exist but one of the parent segments is a file") {
        failOn(
          opt.dir.get("badFileParent", vldNox)
        ) shouldBe s"Directory is uncreatable, $ExistingFile exists: $ExistingFile/cant-exist"
        failOn(
          opt.dir.get("badFileParent", vldNox.withTag("Src"))
        ) shouldBe s"Src is uncreatable, $ExistingFile exists: $ExistingFile/cant-exist"
      }
    }

    describe("when optionally it exists") {
      it("should get when it doesn't exist") { opt.dir.get("nox", vldMaybe) shouldBe NonExistingPath }
      it("should fail when present but the wrong type") {
        failOn(opt.dir.get("file", vldMaybe)) shouldBe s"Expected a directory, found file: $ExistingFile"
        failOn(
          opt.dir.get("file", vldMaybe.withTag("Src"))
        ) shouldBe s"Src expected a directory, found file: $ExistingFile"
      }
      it("should get when it exists as a directory") { opt.dir.get("dir", vldMaybe) shouldBe Tmp }
    }

    describe("when converting other types") {
      it("should fail to convert a string") {
        assume(!(Pwd / "value").exists)
        failOn(opt.dir.getOr("string", DfltDir)) shouldBe s"Directory doesn't exist: $Pwd/value"
        failOn(opt.dir.getOr("string", DfltDir, vldTag)) shouldBe s"Source doesn't exist: $Pwd/value"
      }
      it("should fail to convert a string list") {
        assume(!(Pwd / "x,y").exists)
        failOn(opt.dir.getOr("strings", DfltDir)) shouldBe s"Directory doesn't exist: $Pwd/x,y"
        failOn(opt.dir.getOr("strings", DfltDir, vldTag)) shouldBe s"Source doesn't exist: $Pwd/x,y"
      }
      it("should fail to convert a boolean") {
        assume(!(Pwd / "true").exists)
        failOn(opt.dir.getOr("bool", DfltDir)) shouldBe s"Directory doesn't exist: $Pwd/true"
        failOn(opt.dir.getOr("bool", DfltDir, vldTag)) shouldBe s"Source doesn't exist: $Pwd/true"
      }
      it("should fail to convert a int") {
        assume(!(Pwd / "12345").exists)
        failOn(opt.dir.getOr("int", DfltDir)) shouldBe s"Directory doesn't exist: $Pwd/12345"
        failOn(opt.dir.getOr("int", DfltDir, vldTag)) shouldBe s"Source doesn't exist: $Pwd/12345"
      }
    }
  }
}
