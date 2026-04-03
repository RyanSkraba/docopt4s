package com.tinfoiled.docopt4s.testkit

import org.scalatest.{BeforeAndAfterAll, Suite}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}
import scala.util.{Failure, Using}

/** Trait for creating a temporary directory and deleting it after the suite is done. */
trait TmpDir extends BeforeAndAfterAll { this: Suite =>

  /** The prefix to use for the temporary directory. */
  val Prefix: String = s"TmpDirScalatest_${getClass.getSimpleName}"

  /** A local temporary directory for test file storage. */
  val Tmp: Path = Files.createTempDirectory(Prefix)

  /** The directory that we're being run in (used for relative paths) */
  val Pwd: Path = Paths.get(".").toAbsolutePath.normalize()

  /** A file with a basic scenario. */
  lazy val ExistingFile: Path = {
    val path = nonExisting(Tmp, "existing.txt")
    Files.createFile(path)
    Files.writeString(path, "file", StandardCharsets.UTF_8)
    path
  }

  /** A simple file that is guaranteed not to exist at the start. */
  val NonExistingPath: Path = nonExisting()

  /** Override this to keep the temporary directory after writing. */
  lazy val Keep: Boolean = false

  /** @return a path that is guaranteed not to exist when the method is called */
  def nonExisting(path: Path = Tmp, tag: String = "nox"): Path = {
    val candidate = path.resolve(tag)
    if (!Files.exists(candidate)) return candidate
    LazyList.from(1).map(i => path.resolve(tag + i)).find(p => !Files.exists(p)).get
  }

  /** Delete temporary resources after the script. */
  override protected def afterAll(): Unit = {
    super.afterAll()
    if (!Keep) {
      Using(Files.walk(Tmp)) { str =>
        import scala.jdk.CollectionConverters._
        str.iterator().asScala.toSeq.sortBy(_.getNameCount).reverse.foreach(Files.delete)
      } match {
        case Failure(ex) => ex.printStackTrace()
        case _           =>
      }
    }
  }
}
