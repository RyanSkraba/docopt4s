package com.tinfoiled.docopt4s.testkit.example

import com.tinfoiled.docopt4s.testkit.{WithFileTests, MultiTaskMainSpec}

/** Unit tests for [[FileTestTask]] */
class FileTestTaskSpec extends MultiTaskMainSpec(ExampleGo, Some(FileTestTask)) with WithFileTests {

  describe(s"Standard $MainName $TaskCmd command line help, versions and exceptions") {
    itShouldHandleVersionAndHelpFlags()
    itShouldThrowOnUnknownOptKey()
    itShouldThrowOnIncompleteArgs()
    itShouldThrowOnIncompleteArgs("--file")
    itShouldThrowOnIncompleteArgs("--dir")
    itShouldThrowOnIncompleteArgs("--file", "--exists")
    itShouldThrowOnIncompleteArgs("--dir", "--no-exists")
    itShouldThrowOnMissingOptValue("x", "--tag")
    itShouldThrowOnIncompatibleOpts("--file", "--dir", "X")
    itShouldThrowOnIncompatibleOpts("--exists", "--no-exists", "X")

    itShouldBeAnExistingPath()("--exists", "<>")
    itShouldBeAnExistingFile()("--file", "--exists", "<>")
    itShouldBeAnExistingDir()("--dir", "--exists", "<>")
    itShouldBeANonExistingPath()("--no-exists", "<>")
    itShouldBeANonExistingFile()("--no-exists", "--file", "<>")
    itShouldBeANonExistingDir()("--no-exists", "--dir", "<>")

    itShouldBeAnExistingPath("Src")("--tag", "Src", "--exists", "<>")
    itShouldBeAnExistingFile("Src")("--tag", "Src", "--file", "--exists", "<>")
    itShouldBeAnExistingDir("Src")("--tag", "Src", "--dir", "--exists", "<>")
    itShouldBeANonExistingPath("Src")("--tag", "Src", "--no-exists", "<>")
    itShouldBeANonExistingFile("Src")("--tag", "Src", "--no-exists", "--file", "<>")
    itShouldBeANonExistingDir("Src")("--tag", "Src", "--no-exists", "--dir", "<>")
  }

  describe(s"$MainName $TaskCmd basic scenarios") {
    describe("without specifying whether it exists") {
      it("should find a path") {
        withGoStdout(TaskCmd, Tmp) shouldBe s"OK $Tmp"
        withGoStdout(TaskCmd, ExistingFile) shouldBe s"OK $ExistingFile"
        withGoStdout(TaskCmd, NonExistingPath) shouldBe s"OK $NonExistingPath"
      }
      it("should find a file") {
        interceptGoDocoptEx(TaskCmd, "--file", Tmp).getMessage shouldBe s"Expected a file, found directory: $Tmp"
        withGoStdout(TaskCmd, "--file", ExistingFile) shouldBe s"OK $ExistingFile"
        withGoStdout(TaskCmd, "--file", NonExistingPath) shouldBe s"OK $NonExistingPath"
      }
      it("should find a directory") {
        withGoStdout(TaskCmd, "--dir", Tmp) shouldBe s"OK $Tmp"
        interceptGoDocoptEx(
          TaskCmd,
          "--dir",
          ExistingFile
        ).getMessage shouldBe s"Expected a directory, found file: $ExistingFile"
        withGoStdout(TaskCmd, "--dir", NonExistingPath) shouldBe s"OK $NonExistingPath"
      }
    }

    describe("when it must exist") {
      it("should find a path") {
        withGoStdout(TaskCmd, "--exists", Tmp) shouldBe s"OK $Tmp"
        withGoStdout(TaskCmd, "--exists", ExistingFile) shouldBe s"OK $ExistingFile"
        interceptGoDocoptEx(
          TaskCmd,
          "--exists",
          NonExistingPath
        ).getMessage shouldBe s"Path doesn't exist: $NonExistingPath"
      }
      it("should find a file") {
        interceptGoDocoptEx(
          TaskCmd,
          "--exists",
          "--file",
          Tmp
        ).getMessage shouldBe s"Expected a file, found directory: $Tmp"
        withGoStdout(TaskCmd, "--exists", "--file", ExistingFile) shouldBe s"OK $ExistingFile"
        interceptGoDocoptEx(
          TaskCmd,
          "--exists",
          "--file",
          NonExistingPath
        ).getMessage shouldBe s"File doesn't exist: $NonExistingPath"
      }
      it("should find a directory") {
        withGoStdout(TaskCmd, "--exists", "--dir", Tmp) shouldBe s"OK $Tmp"
        interceptGoDocoptEx(
          TaskCmd,
          "--exists",
          "--dir",
          ExistingFile
        ).getMessage shouldBe s"Expected a directory, found file: $ExistingFile"
        interceptGoDocoptEx(
          TaskCmd,
          "--exists",
          "--dir",
          NonExistingPath
        ).getMessage shouldBe s"Directory doesn't exist: $NonExistingPath"
      }
    }

    describe("when it must not exist") {
      it("should find a path") {
        interceptGoDocoptEx(TaskCmd, "--no-exists", Tmp).getMessage shouldBe s"Directory already exists: $Tmp"
        interceptGoDocoptEx(
          TaskCmd,
          "--no-exists",
          ExistingFile
        ).getMessage shouldBe s"File already exists: $ExistingFile"
        withGoStdout(TaskCmd, "--no-exists", NonExistingPath) shouldBe s"OK $NonExistingPath"
      }
      it("should find a file") {
        interceptGoDocoptEx(TaskCmd, "--no-exists", "--file", Tmp).getMessage shouldBe s"Directory already exists: $Tmp"
        interceptGoDocoptEx(
          TaskCmd,
          "--no-exists",
          "--file",
          ExistingFile
        ).getMessage shouldBe s"File already exists: $ExistingFile"
        withGoStdout(TaskCmd, "--no-exists", "--file", NonExistingPath) shouldBe s"OK $NonExistingPath"
      }
      it("should find a directory") {
        interceptGoDocoptEx(TaskCmd, "--no-exists", "--dir", Tmp).getMessage shouldBe s"Directory already exists: $Tmp"
        interceptGoDocoptEx(
          TaskCmd,
          "--no-exists",
          "--dir",
          ExistingFile
        ).getMessage shouldBe s"File already exists: $ExistingFile"
        withGoStdout(TaskCmd, "--no-exists", "--dir", NonExistingPath) shouldBe s"OK $NonExistingPath"
      }
    }
  }
}
