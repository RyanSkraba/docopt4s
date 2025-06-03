package com.tinfoiled.docopt4s

import scala.jdk.CollectionConverters._
import scala.reflect.io.{Directory, File, Path}
import scala.util.Properties

/** Once created, a [[Docopt]] provides a means to interpret command line arguments.
  *
  * The Docopt text spec is described at https://docopt.org/.
  */
trait Docopt {

  def getStringsOption(key: String): Option[Iterable[String]]

  def getStrings(key: String, default: Iterable[String]): Iterable[String] = getStringsOption(key).getOrElse(default)

  def getStrings(key: String): Iterable[String] = getStringsOption(key).getOrElse {
    throw new DocoptException(s"Expected $key not found")
  }

  def getStringOption(key: String): Option[String]

  def getString(key: String, default: String): String = getStringOption(key).getOrElse(default)

  def getString(key: String): String = getStringOption(key).getOrElse {
    throw new DocoptException(s"Expected $key not found")
  }

  def getIntOption(key: String): Option[Int] = getStringOption(key).map(value =>
    value.toIntOption.getOrElse {
      throw new DocoptException("Expected an integer for $key, but got $value")
    }
  )

  def getInt(key: String, default: Int): Int = getIntOption(key).getOrElse(default)

  def getInt(key: String): Int = getIntOption(key).getOrElse {
    throw new DocoptException(s"Expected $key not found")
  }

  def getBooleanOption(key: String): Option[Boolean]

  def getBoolean(key: String, default: Boolean): Boolean = getBooleanOption(key).getOrElse(default)

  def getBoolean(key: String): Boolean = getBooleanOption(key).getOrElse {
    throw new DocoptException(s"Expected $key not found")
  }

  /** Helper to validate command line arguments against an expected filesystem state.
    *
    * @param root
    *   An absolute directory to use in constructing the path
    * @param tag
    *   A human-readable description to describe the
    * @param ifIsDir
    *   Whether to test to ensure the argument must be a Directory or must be a File (or None if it doesn't matter).
    * @param ifExists
    *   Whether to test to ensure the argument must exist or must not exist (or None if it doesn't matter).
    * @return
    *   The validated path that the argument represents on the filesystem.
    */
  case class PathValidator(
      root: Option[AnyRef] = None,
      systemEnvVar: Option[String] = None,
      tag: Option[String] = None,
      ifIsDir: Option[Boolean] = None,
      ifExists: Option[Boolean] = Some(true)
  ) {

    def validate(key: String): Path = {
      val path: Path = Path(
        root
          .map(_.toString)
          .orElse(systemEnvVar.flatMap(sys.env.get(_)))
          .orElse(Option(Properties.userDir))
          .getOrElse("/")
      )
        .resolve(Path(getString(key)))
        .toAbsolute
      val pathTag = tag.getOrElse(
        ifIsDir match {
          case Some(true)  => "Directory"
          case Some(false) => "File"
          case None        => "Path"
        }
      )
      if (ifExists.contains(true) && !path.exists)
        throw new DocoptException(s"$pathTag doesn't exist: $path")
      if (ifExists.contains(false) && path.exists)
        throw new DocoptException(s"$pathTag already exists: $path")
      if (ifIsDir.contains(true) && ifExists.contains(true) && !path.isDirectory)
        throw new DocoptException(s"$pathTag is not a directory: $path")
      if (ifIsDir.contains(false) && ifExists.contains(true) && !path.isFile)
        throw new DocoptException(s"$pathTag is not a file: $path")
      path
    }

    def withTag(tag: String) = copy(tag = Some(tag))
    def isPath(): PathValidator = copy(ifIsDir = None)
    def isDir(): PathValidator = copy(ifIsDir = Some(true))
    def isFile(): PathValidator = copy(ifIsDir = Some(false))

    def exists(): PathValidator = copy(ifExists = Some(true))
    def doesntExist(): PathValidator = copy(ifExists = Some(false))
    def optionallyExists(): PathValidator = copy(ifExists = None)
  }

  def getPathOption(key: String, vld: PathValidator = PathValidator()): Option[Path] =
    getStringOption(key).map(_ => vld.validate(key))
  def getPath(key: String, vld: PathValidator = PathValidator()): Path = vld.validate(key)

  def getFileOption(key: String, vld: PathValidator = PathValidator()): Option[File] =
    getPathOption(key, vld.isFile()).map(_.toFile)
  def getFile(key: String, vld: PathValidator = PathValidator().isFile()): File = getPath(key, vld.isFile()).toFile

  def getDirectoryOption(key: String, vld: PathValidator = PathValidator()): Option[Directory] =
    getPathOption(key, vld.isDir()).map(_.toDirectory)
  def getDirectory(key: String, vld: PathValidator = PathValidator().isDir()): Directory =
    getPath(key, vld.isDir()).toDirectory
}

object Docopt {

  def apply(doc: String, version: String, args: Iterable[String], optionsFirst: Boolean = false): Docopt = {
    try {
      // Delegate to the Java implementation to obtain a map of arguments
      apply(
        new org.docopt.Docopt(doc)
          .withVersion(version)
          .withOptionsFirst(optionsFirst)
          .withExit(false)
          .parse(args.toList.asJava)
          .asScala
          .toMap
          .filter(_._2 != null) // Remove all null values
      )
    } catch {
      case ex: org.docopt.DocoptExitException =>
        throw new DocoptException(ex.getMessage, ex, doc, ex.getExitCode)
    }
  }

  def apply(argMap: Map[String, Any]): Docopt = {
    new Docopt {
      override def getStringsOption(key: String): Option[Iterable[String]] = argMap.get(key) match {
        case Some(value: String)                     => Some(Seq(value))
        case Some(value: java.lang.Iterable[String]) => Some(value.asScala)
        case Some(value)                             => Some(Seq(value.toString))
        case None                                    => None
      }

      override def getStringOption(key: String): Option[String] = argMap.get(key) match {
        case Some(value: String)                     => Some(value)
        case Some(value: java.lang.Iterable[String]) => Some(value.asScala.mkString(","))
        case Some(value)                             => Some(value.toString)
        case None                                    => None
      }

      override def getBooleanOption(key: String): Option[Boolean] =
        argMap.get(key).map(_.toString.toBoolean)
    }
  }
}
