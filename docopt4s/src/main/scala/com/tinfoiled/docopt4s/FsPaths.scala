package com.tinfoiled.docopt4s

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

/** Filesystem helpers shared with the testkit (same package hierarchy). */
private[docopt4s] object FsPaths {

  def makeTempDir(prefix: String): Path =
    Files.createTempDirectory(prefix)

  def createFile(path: Path): Path = {
    Files.createFile(path)
    path
  }

  def createDirectories(path: Path): Path = {
    Files.createDirectories(path)
    path
  }

  def writeString(path: Path, content: String): Unit =
    Files.writeString(path, content, StandardCharsets.UTF_8)

  def deleteRecursively(path: Path): Unit =
    if (Files.exists(path)) {
      val stream = Files.walk(path)
      try {
        import scala.jdk.CollectionConverters._
        stream.iterator().asScala.toSeq.sortBy(_.getNameCount).reverse.foreach(Files.delete)
      } finally stream.close()
    }
}
