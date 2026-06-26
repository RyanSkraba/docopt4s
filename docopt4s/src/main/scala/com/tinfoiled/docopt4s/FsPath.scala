package com.tinfoiled.docopt4s

import java.io.{BufferedWriter, InputStream}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, OpenOption, Path, Paths}
import scala.jdk.StreamConverters._
import scala.util.{Try, Using}

/** FileSystem helper methods, adapting java.nio.file.Paths to look a bit more like os-lib or scala.reflect.io files
  * classes. This decorates the existing classes with sugary sweet icing.
  */
object FsPath {

  /** The user's home directory. */
  val Home: Path = Paths.get(System.getProperty("user.home"))

  /** The directory that we're being run in (used for relative paths). */
  val Pwd: Path = Paths.get(".").toAbsolutePath.normalize()

  /** Add methods to the java.nio.file.Path.
    *
    * @param path
    *   The instance to adapt.
    */
  implicit class RichPath(val path: Path) extends AnyVal {
    def /(child: String): Path = path.resolve(child)
    def /(child: Path): Path = path.resolve(child)
    def safeSlurp(): Option[String] = Try { slurp() }.toOption
    def slurp(): String = Files.readString(path, StandardCharsets.UTF_8)
    def writeAll(out: String*): Path = Files.writeString(path, out.mkString(""), StandardCharsets.UTF_8)
    def createDirectory(failIfExists: Boolean = true): Path = {
      if (failIfExists) Files.createDirectory(path) else Files.createDirectories(path)
    }
    def createFile(failIfExists: Boolean = true): Path = {
      if (failIfExists || !path.exists()) Files.createFile(path)
      path
    }
    def deleteRecursively(): Unit = if (Files.exists(path)) {
      val stream = Files.walk(path)
      try {
        import scala.jdk.CollectionConverters._
        stream.iterator().asScala.toSeq.sortBy(_.getNameCount).reverse.foreach(Files.delete)
      } finally stream.close()
    }
    def list(): Seq[Path] = Files.list(path).toScala(Seq)
    def files(): Seq[Path] = list().filter(Files.isRegularFile(_))
    def dirs(): Seq[Path] = list().filter(Files.isDirectory(_))
    def name(): String = path.getFileName.toString
    def ext(): String = name().split("\\.").lastOption.getOrElse("")
    def exists(): Boolean = Files.exists(path)
    def inputStream(opts: OpenOption*): InputStream = Files.newInputStream(path, opts: _*)
    def bufferedWriter(opts: OpenOption*): BufferedWriter = Files.newBufferedWriter(path, opts: _*)
    def lines(): Iterator[String] = Using.resource(scala.io.Source.fromFile(path.toFile)) { out =>
      out.getLines().toList.to(Iterator)
    }
  }
}
