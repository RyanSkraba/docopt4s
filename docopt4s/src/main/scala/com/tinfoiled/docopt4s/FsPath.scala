package com.tinfoiled.docopt4s

import java.io.{BufferedReader, BufferedWriter, InputStream, OutputStream}
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

    /** @return the parent of the path */
    def up: Path = path.getParent

    /** @return whether the path exists */
    def exists: Boolean = Files.exists(path)

    /** @return whether the path exists and is a directory */
    def isDirectory: Boolean = Files.isDirectory(path)

    /** @return whether the path exists and is a file */
    def isFile: Boolean = Files.isRegularFile(path)

    /** @return the last segment of the path, typically the filename */
    def name: String = path.getFileName.toString

    /** @return separate the filename into a base name and an extension, if any */
    def baseExt: (String, String) = {
      val nm = name
      val split = name.split("\\.")
      if (split.size > 1) (nm.dropRight(split.last.length + 1), split.last)
      else (nm, "")
    }

    /** @return the list of children paths, if it is a directory */
    def list: Seq[Path] = Files.list(path).toScala(Seq)

    /** @return the list of files in the directory, if it is a directory */
    def files: Seq[Path] = list.filter(_.isFile)

    /** @return the list of subdirectories, if it is a directory */
    def dirs: Seq[Path] = list.filter(_.isDirectory)

    /** @return the path make by resolving the child against this path. */
    def /(child: String): Path = path.resolve(child)

    /** @return the path make by resolving the child against this path. */
    def /(child: Path): Path = path.resolve(child)

    /** Create a directory or directories.
      * @param failIfExists
      *   If true, then only create the directory if it doesn't exist but the parent directory exists. If false, then
      *   attempt to create the directory and all subdirectories whether or not they exist.
      * @return
      *   The path to the directory
      */
    def createDirectory(failIfExists: Boolean = true): Path = {
      if (failIfExists) Files.createDirectory(path) else Files.createDirectories(path)
    }

    /** Create a file with no contents.
      * @param failIfExists
      *   If true, then only create the file if it doesn't exist but the parent directory exists. If false, then attempt
      *   to create the directory and all subdirectories whether or not they exist.
      * @return
      *   The file.
      */
    def createFile(failIfExists: Boolean = true): Path = {
      if (failIfExists || !path.up.exists) path.getParent.createDirectory(failIfExists = false)
      if (failIfExists || !path.exists) Files.createFile(path)
      path
    }

    /** Delete the path and all its contents if a directory
      * @param max
      *   the maximum number of paths to delete, used to prevent accidentally deleting a root or home directory.
      */
    def deleteRecursively(max: Int = 100): Unit = if (Files.exists(path)) {
      Using.resource(Files.walk(path)) { stream =>
        import scala.jdk.CollectionConverters._
        val fs = Files.walk(path).iterator().asScala.toSeq
        if (fs.length > max) throw new UnsupportedOperationException(s"Too many files to delete: ${fs.length} > $max")
        fs.sortBy(_.getNameCount).reverse.foreach(Files.delete)
      }
    }

    /** @param opts
      *   options for opening a file (including whether it can already exist, or appending)
      * @return
      *   an output stream that writes to the file
      */
    def outputStream(opts: OpenOption*): OutputStream = Files.newOutputStream(path, opts: _*)

    /** @param opts
      *   options for opening a file
      * @return
      *   an input stream that reads from the file
      */
    def inputStream(opts: OpenOption*): InputStream = Files.newInputStream(path, opts: _*)

    /** @param opts
      *   options for opening a file (including whether it can already exist, or appending)
      * @return
      *   a buffered writer for writing text to a file
      */
    def bufferedWriter(opts: OpenOption*): BufferedWriter = Files.newBufferedWriter(path, opts: _*)

    /** @return a buffered reader for reading text from a file */
    def bufferedReader(): BufferedReader = Files.newBufferedReader(path)

    /** @return the contents of the file as lines: each element corresponds to a line in the file */
    def lines: Iterator[String] = Using.resource(scala.io.Source.fromFile(path.toFile)) { out =>
      out.getLines().toList.to(Iterator)
    }

    /** @return the contents of the file as a single string if it can be read, or None if any problems occurred. */
    def safeSlurp(): Option[String] = Try { slurp() }.toOption

    /** @return
      *   the contents of the file as a single string if it can be read, throwing an exception for any problems.
      */
    def slurp(): String = Files.readString(path, StandardCharsets.UTF_8)

    /** @param out
      *   The string contents to write to the file
      * @return
      *   the path to the file
      */
    def writeAll(out: String*): Path = Files.writeString(path, out.mkString(""), StandardCharsets.UTF_8)
  }
}
