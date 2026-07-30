package com.tinfoiled.docopt4s.testkit

import com.tinfoiled.docopt4s.FsPath.RichPath
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

import java.nio.file.{Files, Paths}
import scala.jdk.CollectionConverters.IteratorHasAsScala
import scala.util.{Random, Using}

/** Unit tests for [[RandomFiles]] */
class RandomFilesSpec extends AnyFunSpecLike with Matchers with TmpDir {

  describe(s"Using RandomFiles") {

    RandomFiles.fillDirectory(
      new Random(0L),
      dir = Tmp / "random",
      numFiles = 1000,
      minFiles = 0,
      maxFiles = 10,
      maxDirs = 10,
      oneLarge = Some("bigFile.bin", Int.MaxValue / 100),
      time = Some(0)
    )

    it("should create a set of files in the temporary directory") {
      val files = Files.walk(Tmp).iterator().asScala.toSeq.sortBy(_.name)

      files.filter(_.isFile) should have size 1000
      files.filter(_.isDirectory) should have size 799

      // The big file in the batch
      files should contain(Tmp / "random" / "bigFile.bin")
      (Tmp / "random" / "bigFile.bin").toFile.length shouldBe 21_474_836

      // Since we're creating the scenario deterministically with a seed, this file should exist
      val txt = Tmp / "random" / "3Z0EP1uW" / "4SIebeyBk" / "Edna6" / "nvmVfF.txt"
      files should contain(txt)
      txt.toFile.length shouldBe 5263
      txt.slurp().take(20) shouldBe "mRoUO4u3W5RrbChrZti0"

      // As well as this binary file
      val bin = Tmp / "random" / "3Z0EP1uW" / "4SIebeyBk" / "Edna6" / "0n90T06rcb.bin"
      files should contain(bin)
      bin.toFile.length shouldBe 3328
      Using.resource(bin.inputStream()) { _.readNBytes(1000).map(_.toInt).sum } shouldBe 2988
    }
  }
}
