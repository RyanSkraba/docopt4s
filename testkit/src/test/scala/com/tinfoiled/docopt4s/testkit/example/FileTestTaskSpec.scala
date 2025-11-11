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
}
