package com.tinfoiled.docopt4s.testkit.example

import com.tinfoiled.docopt4s.testkit.{FileValidator, MultiTaskMainSpec}

/** Unit tests for [[FileCheckTask]] */
class FileCheckTaskSpec extends MultiTaskMainSpec(ExampleGo, Some(FileCheckTask)) with FileValidator {

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

    itShouldBeAnExistingFile()("--file", "--exists", "<>")
    itShouldBeAnExistingDir()("--dir", "--exists", "<>")

    itShouldBeAnExistingFile("Src")("--tag", "Src", "--file", "--exists", "<>")
    itShouldBeAnExistingDir("Src")("--tag", "Src", "--dir", "--exists", "<>")
  }
}
