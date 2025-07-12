package com.tinfoiled.docopt4s.testkit.example

import com.tinfoiled.docopt4s.testkit.TmpDir
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

/** Unit tests for [[TmpDir]] */
class TmpDirSpec extends AnyFunSpecLike with Matchers with TmpDir {

  override def afterAll(): Unit = {
    super.afterAll()
    // Check that the cleanup succeeded
    Tmp.jfile shouldNot exist
  }

  describe(s"Using TmpDir") {
    it("should create a temporary directory and some resources") {
      Tmp.jfile should exist
      Pwd.jfile should exist

      // An existing file and a non-existing file
      ExistingFile.jfile should exist
      NonExistingPath.jfile shouldNot exist
      NonExistingPath.name shouldBe "nox"

      // Creating new non-existing files by incrementing
      NonExistingPath.toFile.writeAll("")
      NonExistingPath.jfile should exist
      nonExisting(Tmp).jfile shouldNot exist
      nonExisting(Tmp).name shouldBe "nox1"
    }
  }
}
