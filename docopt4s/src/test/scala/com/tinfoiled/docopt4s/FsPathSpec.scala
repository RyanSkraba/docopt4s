package com.tinfoiled.docopt4s

import com.tinfoiled.docopt4s.FsPath.RichPath
import org.scalatest.BeforeAndAfterAll
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

import java.io.{FileNotFoundException, IOException}
import java.nio.charset.StandardCharsets
import java.nio.file.{
  FileAlreadyExistsException,
  FileSystemException,
  Files,
  NoSuchFileException,
  Path,
  Paths,
  StandardOpenOption
}
import scala.util.Using

/** Test the [[FsPath]] helper. */
class FsPathSpec extends AnyFunSpecLike with BeforeAndAfterAll with Matchers {

  /** We can't use TmpDir in this project directly, but we use the same prefix */
  val Prefix: String = s"TmpDirScalatest_${getClass.getSimpleName}"
  val Tmp: Path = Files.createTempDirectory(Prefix)

  // Use a different subdirectory for each test in order to keep them independent.
  val subdir1 = (Tmp / "subdir1").createDirectory()
  val subdir2 = (Tmp / "subdir2").createDirectory()
  val subdir3 = (Tmp / "subdir3").createDirectory()
  val subdir4 = (Tmp / "subdir4").createDirectory()
  (Tmp / "file1").writeAll("file1")
  (Tmp / "file2").writeAll("file1")

  /** Delete temporary resources after the script. */
  override protected def afterAll(): Unit = {
    super.afterAll()
    Tmp.deleteRecursively()
  }

  describe("Test the rich path methods") {

    it("should have the current working directory") {
      FsPath.Pwd.toFile should exist
      FsPath.Pwd.exists shouldBe true
      FsPath.Pwd.isDirectory shouldBe true
      FsPath.Pwd.isFile shouldBe false
    }

    it("should have the home directory") {
      FsPath.Home.toFile should exist
      FsPath.Home.exists shouldBe true
      FsPath.Home.isDirectory shouldBe true
      FsPath.Home.isFile shouldBe false
    }

    it("should test whether paths exist") {
      (Tmp / "subdir1").toFile should exist
      (Tmp / "file1").toFile should exist
      (Tmp / Paths.get("subdir1/")).toFile should exist
      (Tmp / Paths.get("file1")).toFile should exist
      (Tmp / "nox").toFile shouldNot exist

      (Tmp / "subdir1").exists shouldBe true
      (Tmp / "file1").exists shouldBe true
      (Tmp / Paths.get("subdir1/")).exists shouldBe true
      (Tmp / Paths.get("file1")).exists shouldBe true
      (Tmp / "nox").exists shouldBe false
    }

    it("should test whether paths are files or directories") {
      (Tmp / "subdir1").isFile shouldBe false
      (Tmp / "file1").isFile shouldBe true
      (Tmp / "nox").isFile shouldBe false

      (Tmp / "subdir1").isDirectory shouldBe true
      (Tmp / "file1").isDirectory shouldBe false
      (Tmp / "nox").isDirectory shouldBe false
      (Tmp / "nox/").isDirectory shouldBe false
    }

    it("should split filenames to base and extensions") {
      (Tmp / "filename.ext").name shouldBe "filename.ext"
      (Tmp / "filename.ext").baseExt shouldBe ("filename", "ext")
      (Tmp / "filename").baseExt shouldBe ("filename", "")
      (Tmp / "filename.").baseExt shouldBe ("filename.", "")
    }

    it("should list children files and directories") {
      Tmp.list should have size 6
      Tmp.files should have size 2
      Tmp.dirs should have size 4
    }

    it("should create directories") {
      // Fail when it already exists
      intercept[FileAlreadyExistsException] { subdir1.createDirectory() } should have message subdir1.toString

      // Ignore if it already exists if specified
      subdir1.createDirectory(failIfExists = false)

      // Fail when it can't be created because it exists as a file
      intercept[FileAlreadyExistsException] {
        (Tmp / "file1").createDirectory()
      } should have message (Tmp / "file1").toString

      // Fail when it can't be created because the parent doesn't exist or is a file
      intercept[FileSystemException] {
        (subdir1 / "nox" / "subdir").createDirectory()
      } should have message s"${subdir1 / "nox" / "subdir"}"
      intercept[FileSystemException] {
        (Tmp / "file1" / "subdir").createDirectory()
      } should have message s"${Tmp / "file1" / "subdir"}: Not a directory"
      intercept[FileSystemException] {
        (Tmp / "file1" / "subdir").createDirectory(failIfExists = false)
      } should have message s"${Tmp / "file1" / "subdir"}: Not a directory"

      // Succeed if the parent exists or if failIfExists is set to false
      (subdir1 / "subdir1.1").createDirectory().toFile should exist
      (subdir1 / "subdir1.1").createDirectory(failIfExists = false).toFile should exist
      (subdir1 / "subdir1.2").createDirectory(failIfExists = false).toFile should exist
      (subdir1 / "subdir1.3" / "subdir1.3.1").createDirectory(failIfExists = false).toFile should exist

      subdir1.list should have size 3
      (subdir1 / "subdir1.1").list shouldBe empty
      (subdir1 / "subdir1.2").list shouldBe empty
      (subdir1 / "subdir1.3").list should have size 1
    }

    it("should create files") {
      val file1 = Tmp / "file1"

      // Fail when it already exists
      intercept[FileAlreadyExistsException] { file1.createFile() } should have message file1.toString

      // Fail when it can't be created because it exists as a directory
      intercept[FileAlreadyExistsException] { subdir2.createFile() } should have message subdir2.toString

      // Fail when it can't be created because the parent doesn't exist or is a file
      intercept[FileSystemException] {
        (subdir2 / "nox" / "file").createDirectory()
      } should have message s"${subdir2 / "nox" / "file"}"
      intercept[FileSystemException] {
        (Tmp / "file1" / "file").createDirectory()
      } should have message s"${Tmp / "file1" / "file"}: Not a directory"
      intercept[FileSystemException] {
        (Tmp / "file1" / "file").createDirectory(failIfExists = false)
      } should have message s"${Tmp / "file1" / "file"}: Not a directory"

      // Succeed if the parent exists and if failIfExists is set to false
      (subdir2 / "file2.1").createFile().toFile should exist
      (subdir2 / "file2.1").createFile(failIfExists = false).toFile should exist
      (subdir2 / "file2.2").createFile(failIfExists = false).toFile should exist
      (subdir2 / "subdir2.2" / "file2.2.1").createFile(failIfExists = false).toFile should exist
    }

    it("should delete files and directories") {
      // Silently ignore when the path doesn't exist
      (subdir3 / "nox").deleteRecursively()

      // Delete files, directories and subdirectories
      (subdir3 / "file3.1").createFile()
      (subdir3 / "subdir3.1").createFile()
      (subdir3 / "subdir3.2" / "subdir3.2.1" / "subdir3.2.1.1").createDirectory(failIfExists = false)
      (subdir3 / "x" / "x" / "x" / "x" / "x" / "x" / "x" / "x").createFile(failIfExists = false)
      subdir3.list should have size 4

      (subdir3 / "file3.1").deleteRecursively()
      (subdir3 / "subdir3.1").deleteRecursively()
      (subdir3 / "subdir3.2").deleteRecursively()
      subdir3.list should have size 1

      // Fail if we're deleting too many files
      intercept[UnsupportedOperationException] {
        (subdir3 / "x").deleteRecursively(max = 0)
      } should have message "Too many files to delete: 8 > 0"
      intercept[UnsupportedOperationException] {
        (subdir3 / "x").deleteRecursively(max = 7)
      } should have message "Too many files to delete: 8 > 7"
      (subdir3 / "x").deleteRecursively(max = 8)

      subdir3.list shouldBe empty
    }

    it("should help reading and writing to files") {
      // Error reading and writing to a directory
      intercept[FileSystemException] { subdir4.outputStream() } should have message s"$subdir4: Is a directory"
      intercept[FileSystemException] { subdir4.bufferedWriter() } should have message s"$subdir4: Is a directory"
      intercept[IOException] { subdir4.inputStream().readAllBytes() } should have message "Is a directory"
      intercept[IOException] { subdir4.bufferedReader().readLine() } should have message "Is a directory"

      // Use the buffered writer and output stream to write a message
      Using.resource((subdir4 / "a").outputStream()) { out => out.write("Hello".getBytes(StandardCharsets.UTF_8)) }
      Using.resource((subdir4 / "a").outputStream(StandardOpenOption.APPEND)) { out =>
        out.write(" world!".getBytes(StandardCharsets.UTF_8))
      }
      Using.resource((subdir4 / "b").bufferedWriter()) { out => out.write("Hello") }
      Using.resource((subdir4 / "b").bufferedWriter(StandardOpenOption.APPEND)) { out => out.write(" world!") }

      // And reading with an input stream or buffered reader
      Using.resource((subdir4 / "a").inputStream()) { out =>
        new String(out.readAllBytes(), StandardCharsets.UTF_8)
      } shouldBe "Hello world!"
      Using.resource((subdir4 / "b").inputStream()) { out =>
        new String(out.readAllBytes(), StandardCharsets.UTF_8)
      } shouldBe "Hello world!"
      Using.resource((subdir4 / "a").bufferedReader()) { out => out.readLine() } shouldBe "Hello world!"
      Using.resource((subdir4 / "b").bufferedReader()) { out => out.readLine() } shouldBe "Hello world!"

      // Overwrite and read with the helper methods
      (subdir4 / "a").writeAll("Good night\nEveryone")
      (subdir4 / "a").lines.toSeq shouldBe Seq("Good night", "Everyone")
      (subdir4 / "a").slurp() shouldBe "Good night\nEveryone"
      (subdir4 / "a").safeSlurp() shouldBe Some("Good night\nEveryone")

      // Errors with invalid and non-existent files
      intercept[FileSystemException] {
        subdir4.writeAll("Good night\nEveryone")
      } should have message s"$subdir4: Is a directory"
      intercept[FileNotFoundException] { subdir4.lines } should have message s"$subdir4 (Is a directory)"
      intercept[IOException] { subdir4.slurp() } should have message "Is a directory"
      intercept[FileNotFoundException] {
        (subdir4 / "nox").lines
      } should have message s"${subdir4 / "nox"} (No such file or directory)"
      intercept[NoSuchFileException] { (subdir4 / "nox").slurp() } should have message s"${subdir4 / "nox"}"

      // Invalid and non-existent files are ignored with safeSlurp
      (subdir4).safeSlurp() shouldBe None
      (subdir4 / "nox").safeSlurp() shouldBe None

    }
  }
}
