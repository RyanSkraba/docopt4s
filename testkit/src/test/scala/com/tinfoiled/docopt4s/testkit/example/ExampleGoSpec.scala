package com.tinfoiled.docopt4s.testkit.example

import com.tinfoiled.docopt4s.DocoptException
import com.tinfoiled.docopt4s.testkit.MultiTaskMainSpec

/** Unit tests for [[ExampleGo]] */
class ExampleGoSpec extends MultiTaskMainSpec(ExampleGo) {
  describe(s"Standard $MainName command line help, versions and exceptions") {
    itShouldHandleHelpAndVersionFlags()
    itShouldThrowOnMissingTaskCommand("--debug")
    itShouldThrowOnUnknownTaskCommand()
  }
}
