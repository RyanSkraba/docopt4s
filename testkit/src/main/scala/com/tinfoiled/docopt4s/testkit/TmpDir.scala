package com.tinfoiled.docopt4s.testkit

import org.scalatest.{BeforeAndAfterAll, Suite}

import scala.reflect.io.{Directory, File, Path}

/** Trait for creating a temporary directory and deleting it after the suite is done.  */
trait TmpDir extends BeforeAndAfterAll {this: Suite =>

  /** A local temporary directory for test file storage. */
  val Tmp: Directory = Directory.makeTemp(getClass.getSimpleName)

  /** The directory that we're being run in (used for relative paths) */
  val Pwd: Directory = Directory(".").toCanonical.toDirectory

  /** A file with a basic scenario. */
  val ExistingFile: File = (Tmp / "file.txt").createFile()
  ExistingFile.writeAll("file")

  /** A simple file that is guaranteed not to exist at the start. */
  val NonExistingPath: Path = nonExisting(Tmp)

  /** @return a path that is guaranteed not to exist when the method is called */
  def nonExisting(path: Directory, tag: String="nox"): Path = {
    if(!(path/tag).exists) return path / tag
    LazyList.from(1).map(tag+_).map(path/_).filterNot(_.exists).head
  }

  /** Delete temporary resources after the script. */
  override protected def afterAll(): Unit = {
    super.afterAll()
    try { Tmp.deleteRecursively() }
    catch { case ex: Exception => ex.printStackTrace() }
  }
}
