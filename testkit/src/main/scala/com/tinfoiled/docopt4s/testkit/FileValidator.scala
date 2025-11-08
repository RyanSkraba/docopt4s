package com.tinfoiled.docopt4s.testkit

import com.tinfoiled.docopt4s.Task

/** Additional validators for files. */
trait FileValidator extends TmpDir { this: MultiTaskMainSpec[? <: Task] =>

  class PathAdapter(thunk: (Option[String], String, Seq[Any]) => Unit)
      extends ((Option[String], String) => Function[Seq[String], Unit]) {
    def apply()(in: Any*): Unit = thunk(None, "<>", in)
    def apply(tag: String)(in: Any*): Unit = thunk(Some(tag), "<>", in)
    override def apply(tag: Option[String] = None, holder: String = "<>"): Function[Seq[Any], Unit] =
      thunk(tag, holder, _)
  }

  // TODO: Document
  val itShouldBeAnExistingPath: PathAdapter = new PathAdapter((tag, holder, args) => {
    val nonExistArgs = Task.map(_.Cmd).toSeq ++ args.map(arg => if (arg == holder) NonExistingPath else arg)
    it("throws an exception when the path doesn't exist: " + nonExistArgs.mkString(" ")) {
      val t = interceptGoDocoptEx(nonExistArgs: _*)
      t.docopt shouldBe Doc
      t.getMessage shouldBe s"${tag.getOrElse("Path")} doesn't exist: $NonExistingPath"
    }
  })

  // TODO: Document
  val itShouldBeAnExistingFile: PathAdapter = new PathAdapter((tag, holder, args) => {
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

  // TODO: Document
  val itShouldBeAnExistingDir: PathAdapter = new PathAdapter((tag, holder, args) => {
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
}
