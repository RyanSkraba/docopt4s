package com.tinfoiled.docopt4s.testkit

import com.tinfoiled.docopt4s.Task

/** Provide additional unit tests for running the multitool with path, file or directory option values, checking that
  * correct error messages are generated when unexpected values are provided.
  *
  * This can be used to test much of the expected behaviour when "bad" option values are passed in.
  *
  * {{{
  * // Ensure that the --inputDir option value is a directory and that it exists.
  * // This is done by creating tests that replace the argument <> with values that
  * // are known to be invalid, and checking the expected error
  * itShouldBeAnExistingDir()("--inputDir", "<>", "--outputFile", "/tmp/output.txt")
  *
  * // Ensure that the --output option value is a path that does not exist and is creatable.
  * // This is done by creating tests that replace the argument <> with values that
  * // are known to be invalid, and checking the expected error
  * itShouldBeANonExistingFile()("--inputDir", "/tmp/", "--outputFile", "<>")
  * }}}
  *
  * Note: These helpers only test the error conditions, and do not run any tests with the actual, expected option values
  * (such as providing an existing file, when the file is expected to exist). A well-behaved tool should validate the
  * options before performing any actions.
  */
trait WithFileTests extends TmpDir { this: MultiTaskMainSpec[? <: Task] =>

  /** Helper to adapt the arguments to the test. */
  class WithFileAdapter(thunk: (Option[String], String, Seq[Any]) => Unit)
      extends ((Option[String], String) => Function[Seq[String], Unit]) {
    // Note that Scala 3 can overload methods based on multiple argument lists.
    def apply()(in: Any*): Unit = thunk(None, "<>", in)
    def apply(tag: String)(in: Any*): Unit = thunk(Some(tag), "<>", in)
    override def apply(tag: Option[String] = None, holder: String = "<>"): Function[Seq[Any], Unit] =
      thunk(tag, holder, _)
  }

  /** Builds the WithFileAdapter that can run tests for paths, files and directories that should not already exist.
    *
    * @param dfltTag
    *   The default tag to use to describe the resource if it's untagged ("Path", "File" or "Directory"), used in error
    *   messages.
    */
  private[this] def shouldBeNonExisting(dfltTag: String): WithFileAdapter = new WithFileAdapter((tag, holder, args) => {
    val existFileArgs = Task.map(_.Cmd).toSeq ++ args.map(arg => if (arg == holder) ExistingFile else arg)
    it("throws an exception when the file already exists: " + existFileArgs.mkString(" ")) {
      val t = interceptGoDocoptEx(existFileArgs: _*)
      t.docopt shouldBe Doc
      t.getMessage shouldBe s"${tag.getOrElse("File")} already exists: $ExistingFile"
    }

    val existDirArgs = Task.map(_.Cmd).toSeq ++ args.map(arg => if (arg == holder) Tmp else arg)
    it("throws an exception when the directory already exists: " + existDirArgs.mkString(" ")) {
      val t = interceptGoDocoptEx(existDirArgs: _*)
      t.docopt shouldBe Doc
      t.getMessage shouldBe s"${tag.getOrElse("Directory")} already exists: $Tmp"
    }

    val uncreatableArgs = Task.map(_.Cmd).toSeq ++ args.map(arg => if (arg == holder) ExistingFile / "nox" else arg)
    it("throws an exception when the path can't be created: " + uncreatableArgs.mkString(" ")) {
      val t = interceptGoDocoptEx(uncreatableArgs: _*)
      t.docopt shouldBe Doc
      t.getMessage shouldBe s"${tag.getOrElse(dfltTag)} is uncreatable, $ExistingFile exists: $ExistingFile/nox"
    }
  })

  /** Adds a test to ensure that when the tool is run with the given arguments, it will fail because one of the
    * arguments was not an existing path. Use "<>" to specify the argument that should be replaced during the test.
    *
    * {{{
    * itShouldBeAnExistingPath()("--input", "<>")
    * }}}
    */
  val itShouldBeAnExistingPath: WithFileAdapter = new WithFileAdapter((tag, holder, args) => {
    val nonExistArgs = Task.map(_.Cmd).toSeq ++ args.map(arg => if (arg == holder) NonExistingPath else arg)
    it("throws an exception when the path doesn't exist: " + nonExistArgs.mkString(" ")) {
      val t = interceptGoDocoptEx(nonExistArgs: _*)
      t.docopt shouldBe Doc
      t.getMessage shouldBe s"${tag.getOrElse("Path")} doesn't exist: $NonExistingPath"
    }
  })

  /** Adds a test to ensure that when the tool is run with the given arguments, it will fail because one of the
    * arguments already exists as a file or directory, OR that that argument is uncreatable because one of its parent
    * segments exist as a file. Use "<>" to specify the argument that should be replaced during the test.
    *
    * {{{
    * itShouldBeANonExistingPath()("--output", "<>")
    * }}}
    */
  val itShouldBeANonExistingPath: WithFileAdapter = shouldBeNonExisting("Path")

  /** Adds a test to ensure that when the tool is run with the given arguments, it will fail because one of the
    * arguments either does not exist, or exists but isn't a file. Use "<>" to specify the argument that should be
    * replaced during the test.
    *
    * {{{
    * itShouldBeAnExistingFile()("--inputFile", "<>")
    * }}}
    */
  val itShouldBeAnExistingFile: WithFileAdapter = new WithFileAdapter((tag, holder, args) => {
    val nonExistArgs = Task.map(_.Cmd).toSeq ++ args.map(arg => if (arg == holder) NonExistingPath else arg)
    it("throws an exception when the file doesn't exist: " + nonExistArgs.mkString(" ")) {
      val t = interceptGoDocoptEx(nonExistArgs: _*)
      t.docopt shouldBe Doc
      t.getMessage shouldBe s"${tag.getOrElse("File")} doesn't exist: $NonExistingPath"
    }

    val dirArgs = Task.map(_.Cmd).toSeq ++ args.map(arg => if (arg == holder) Tmp else arg)
    it("throws an when the file exists but is a directory: " + dirArgs.mkString(" ")) {
      val t = interceptGoDocoptEx(dirArgs: _*)
      t.docopt shouldBe Doc
      t.getMessage shouldBe tag
        .map(t => s"$t expected a file, found directory: $Tmp")
        .getOrElse(s"Expected a file, found directory: $Tmp")
    }
  })

  /** Adds a test to ensure that when the tool is run with the given arguments, it will fail because one of the
    * arguments already exists as a file or directory, OR that that argument is uncreatable because one of its parent
    * segments exist as a file. Use "<>" to specify the argument that should be replaced during the test.
    *
    * {{{
    * itShouldBeANonExistingFile()("--outputFile", "<>")
    * }}}
    */
  val itShouldBeANonExistingFile: WithFileAdapter = shouldBeNonExisting("File")

  // TODO: Document
  val itShouldBeAnExistingDir: WithFileAdapter = new WithFileAdapter((tag, holder, args) => {
    val nonExistArgs = Task.map(_.Cmd).toSeq ++ args.map(arg => if (arg == holder) NonExistingPath else arg)
    it("throws an exception when the directory doesn't exist: " + nonExistArgs.mkString(" ")) {
      val t = interceptGoDocoptEx(nonExistArgs: _*)
      t.docopt shouldBe Doc
      t.getMessage shouldBe s"${tag.getOrElse("Directory")} doesn't exist: $NonExistingPath"
    }

    val fileArgs = Task.map(_.Cmd).toSeq ++ args.map(arg => if (arg == holder) ExistingFile else arg)
    it("throws an when the directory exists but is a file: " + fileArgs.mkString(" ")) {
      val t = interceptGoDocoptEx(fileArgs: _*)
      t.docopt shouldBe Doc
      t.getMessage shouldBe tag
        .map(t => s"$t expected a directory, found file: $ExistingFile")
        .getOrElse(s"Expected a directory, found file: $ExistingFile")
    }
  })

  /** Adds a test to ensure that when the tool is run with the given arguments, it will fail because one of the
    * arguments already exists as a file or directory, OR that that argument is uncreatable because one of its parent
    * segments exist as a file. Use "<>" to specify the argument that should be replaced during the test.
    *
    * {{{
    * itShouldBeANonExistingFile()("--outputDir", "<>")
    * }}}
    */
  val itShouldBeANonExistingDir: WithFileAdapter = shouldBeNonExisting("Directory")
}
