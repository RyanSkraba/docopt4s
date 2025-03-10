package com.tinfoiled.docopt4s.testkit.example

import com.tinfoiled.docopt4s.DocoptException
import com.tinfoiled.docopt4s.testkit.MultiTaskMainSpec

/** Unit tests for [[ExampleGo]] */
class ExampleGoSpec extends MultiTaskMainSpec(ExampleGo) {

  describe(s"${Main.Name} command line") {

    itShouldThrowOnHelpAndVersionFlags()

    it("throw an exception like --help when run without a command") {
      val t = interceptGoDocoptEx("--debug")
      t.getMessage shouldBe "Missing command"
      t.docopt shouldBe Main.Doc
    }

    it("throws an exception with an unknown command") {
      val t = intercept[DocoptException] {
        withGo("garbage")
      }
      t.getMessage shouldBe "Unknown command: garbage"
      t.docopt shouldBe Main.Doc
    }
  }
}
