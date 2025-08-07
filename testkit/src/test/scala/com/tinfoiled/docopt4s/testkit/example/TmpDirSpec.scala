package com.tinfoiled.docopt4s.testkit.example

import com.tinfoiled.docopt4s.testkit.TmpDir
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

import java.util.prefs.Preferences

/** Unit tests for [[TmpDir]] */
class TmpDirSpec extends AnyFunSpecLike with Matchers with TmpDir {

  override def afterAll(): Unit = {
    Tmp.jfile should exist
    super.afterAll()
    // Check that the cleanup succeeded
    Tmp.jfile shouldNot exist
  }

  describe(s"Using TmpDir") {
    it("should create a temporary directory") { Tmp.jfile should exist }

    it("should be named after the class") {
      Tmp.name should startWith(Prefix)
      Tmp.name should include(getClass.getSimpleName)
    }

    it("should create a resource when requested") {
      // The file doesn't exist until it is used.
      Tmp.list shouldBe empty
      ExistingFile.jfile should exist
      Tmp.list should have size 1
    }

    it("should paths that don't exist on demand") {
      NonExistingPath.jfile shouldNot exist
      NonExistingPath.name shouldBe "nox"

      // Creating new non-existing files by incrementing
      NonExistingPath.toFile.writeAll("")
      NonExistingPath.jfile should exist
      nonExisting().jfile shouldNot exist
      nonExisting().name shouldBe "nox1"
    }
  }
}

/** Unit tests for [[TmpDir]] with [[TmpDir.Keep]] set to True */
class TmpDirKeepSpec extends AnyFunSpecLike with Matchers with TmpDir {

  override lazy val Keep: Boolean = true;

  describe(s"Using TmpDir") {
    it("should create a temporary directory") {
      Tmp.jfile should exist
    }
  }

  override def afterAll(): Unit = {
    Tmp.jfile should exist
    super.afterAll()
    // When Keep is true, the temporary file shouldn't be deleted at the end.
    Tmp.jfile should exist
    // But we'll clean it up manually anyway.
    try { Tmp.deleteRecursively() }
    catch { case ex: Exception => ex.printStackTrace() }
    Tmp.jfile shouldNot exist
  }
}
