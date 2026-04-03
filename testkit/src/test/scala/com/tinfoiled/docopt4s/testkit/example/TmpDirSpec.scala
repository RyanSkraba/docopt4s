package com.tinfoiled.docopt4s.testkit.example

import com.tinfoiled.docopt4s.testkit.TmpDir
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

import java.nio.charset.StandardCharsets
import java.nio.file.Files
import scala.util.{Failure, Using}

/** Unit tests for [[TmpDir]] */
class TmpDirSpec extends AnyFunSpecLike with Matchers with TmpDir {

  override def afterAll(): Unit = {
    Files.exists(Tmp) shouldBe true
    super.afterAll()
    // Check that the cleanup succeeded
    Files.exists(Tmp) shouldBe false
  }

  describe(s"Using TmpDir") {
    it("should create a temporary directory") { Files.exists(Tmp) shouldBe true }

    it("should be named after the class") {
      Tmp.getFileName.toString should startWith(Prefix)
      Tmp.getFileName.toString should include(getClass.getSimpleName)
    }

    it("should create a resource when requested") {
      // The file doesn't exist until it is used.
      Files.list(Tmp).count() shouldBe 0
      Files.exists(ExistingFile) shouldBe true
      Files.list(Tmp).count() shouldBe 1
    }

    it("should paths that don't exist on demand") {
      Files.exists(NonExistingPath) shouldBe false
      NonExistingPath.getFileName.toString shouldBe "nox"

      // Creating new non-existing files by incrementing
      Files.writeString(NonExistingPath, "", StandardCharsets.UTF_8)
      Files.exists(NonExistingPath) shouldBe true
      Files.exists(nonExisting()) shouldBe false
      nonExisting().getFileName.toString shouldBe "nox1"
    }
  }
}

/** Unit tests for [[TmpDir]] with [[TmpDir.Keep]] set to True */
class TmpDirKeepSpec extends AnyFunSpecLike with Matchers with TmpDir {

  override lazy val Keep: Boolean = true

  describe(s"Using TmpDir") {
    it("should create a temporary directory") {
      Files.exists(Tmp) shouldBe true
    }
  }

  override def afterAll(): Unit = {
    Files.exists(Tmp) shouldBe true
    super.afterAll()
    // When Keep is true, the temporary file shouldn't be deleted at the end.
    Files.exists(Tmp) shouldBe true
    // But we'll clean it up manually anyway.
    Using(Files.walk(Tmp)) { str =>
      import scala.jdk.CollectionConverters._
      str.iterator().asScala.toSeq.sortBy(_.getNameCount).reverse.foreach(Files.delete)
    } match {
      case Failure(ex) => ex.printStackTrace()
      case _           =>
    }
    Files.exists(Tmp) shouldBe false
  }
}
