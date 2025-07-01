package com.tinfoiled.docopt4s

import scala.jdk.CollectionConverters._
import scala.reflect.io.{Directory, File, Path}
import scala.util.Properties

/** A [[Docopt]] provides a means to interpret command line arguments.
  *
  * The Docopt text spec is described at https://docopt.org/.
  */
trait Docopt {

  /** Get argument values a String */
  val string: DocoptGet[String]

  /** Get argument values as a String list */
  val strings: DocoptGet[Iterable[String]]

  /** Get argument values a boolean */
  val boolean: DocoptGet[Boolean]

  /** Get argument values an Int */
  val int: DocoptGet[Int] = (key: String) =>
    string
      .getOption(key)
      .map(value =>
        value.toIntOption.getOrElse {
          throw new DocoptException(s"Expected an integer for $key, but got $value")
        }
      )

  /** Get argument values as a Path */
  val path: PathDocoptGet[Path] = new PathDocoptGet[Path] {
    override def getOption(key: String, vld: PathValidator): Option[Path] =
      string.getOption(key).map(_ => vld.validate(string.get(key)))
  }

  def getFileOption(key: String, vld: PathValidator = PathValidator()): Option[File] =
    path.getOption(key, vld.isFile).map(_.toFile)
  def getFileOr(key: String, default: File, vld: PathValidator = PathValidator()): File =
    getFileOption(key, vld).getOrElse(default)
  def getFile(key: String, vld: PathValidator = PathValidator()): File = path.get(key, vld.isFile).toFile

  def getDirectoryOption(key: String, vld: PathValidator = PathValidator()): Option[Directory] =
    path.getOption(key, vld.isDir).map(_.toDirectory)
  def getDirectoryOr(key: String, default: Directory, vld: PathValidator = PathValidator()): Directory =
    getDirectoryOption(key, vld).getOrElse(default)
  def getDirectory(key: String, vld: PathValidator = PathValidator()): Directory =
    path.get(key, vld.isDir).toDirectory
}

/** Gets options of a certain type from the command line arguments.
  *
  * @tparam T
  *   The expected type of the command line argument.
  */
trait DocoptGet[T] {

  /** @param key
    *   The option key for the argument
    * @return
    *   If present and can be transformed to the type, return the transformed argument value or [[None]] if not present.
    *   If it is present but can't be converted or is invalid, throws a DocoptException.
    */
  def getOption(key: String): Option[T]

  /** @param key
    *   The option key for the argument
    * @param default
    *   The default value for the argument if it is not present.
    * @return
    *   If present and can be transformed to the type, return the transformed argument value or use the default if not
    *   present. If it is present but can't be converted or is invalid, throws a DocoptException.
    */
  def get(key: String, default: T): T = getOption(key).getOrElse(default)

  /** @param key
    *   The option key for the argument
    * @return
    *   If present and can be transformed to the type, return the transformed argument value. If it is not present, or
    *   present but can't be converted or is invalid, throws a DocoptException.
    */
  def get(key: String): T = getOption(key).getOrElse { throw new DocoptException(s"Expected $key not found") }
}

/** Gets path options of a certain type from the command line arguments.
  *
  * @tparam T
  *   The expected type of the command line argument.
  */
trait PathDocoptGet[T] {

  val DefaultVld: PathValidator = PathValidator().isPath

  /** @param key
    *   The option key for the argument
    * @param vld
    *   The validator used to check whether the path value exists, and/or is of the right type.
    * @return
    *   If present and can be transformed to the type, return the transformed argument value or [[None]] if not present.
    *   If it is present but can't be converted or is invalid, throws a DocoptException.
    */
  def getOption(key: String, vld: PathValidator = DefaultVld): Option[T]

  /** @param key
    *   The option key for the argument
    * @param default
    *   The default value for the argument if it is not present.
    * @param vld
    *   The validator used to check whether the path value exists, and/or is of the right type.
    * @return
    *   If present and can be transformed to the type, return the transformed argument value or use the default if not
    *   present. If it is present but can't be converted or is invalid, throws a DocoptException.
    */
  def getOr(key: String, default: T, vld: PathValidator = DefaultVld): T = getOption(key, vld).getOrElse(default)

  /** @param key
    *   The option key for the argument
    * @param vld
    *   The validator used to check whether the path value exists, and/or is of the right type.
    * @return
    *   If present and can be transformed to the type, return the transformed argument value. If it is not present, or
    *   present but can't be converted or is invalid, throws a DocoptException.
    */
  def get(key: String, vld: PathValidator = DefaultVld): T = getOption(key, vld).getOrElse {
    throw new DocoptException(s"Expected $key not found")
  }
}

/** Helper to validate command line arguments against an expected filesystem state.
  *
  * @param root
  *   An absolute directory to use in constructing the path
  * @param tag
  *   A human-readable description to describe the argument, used in exceptions.
  * @param ifIsDir
  *   Whether to test to ensure the argument must be a Directory or must be a File (or None if it doesn't matter).
  * @param ifExists
  *   Whether to test to ensure the argument must exist or must not exist (or None if it doesn't matter).
  */
case class PathValidator(
    root: Option[AnyRef] = None,
    systemEnvVar: Option[String] = None,
    tag: Option[String] = None,
    ifIsDir: Option[Boolean] = None,
    ifExists: Option[Boolean] = Some(true)
) {

  private lazy val pathTag: String = tag.getOrElse(
    ifIsDir match {
      case Some(true)  => "Directory"
      case Some(false) => "File"
      case None        => "Path"
    }
  )

  /** @param value
    *   The path to validate as a string.
    * @return
    *   The absolute, validated path that the argument represents on the filesystem.
    */
  def validate(value: String): Path = {
    val path: Path = Path(
      root
        .map(_.toString)
        .orElse(systemEnvVar.flatMap(sys.env.get(_)))
        .orElse(Option(Properties.userDir))
        .getOrElse("/")
    )
      .resolve(Path(value))
      .toAbsolute

    (ifExists, ifIsDir, tag, path.exists) match {
      case (Some(true), _, _, false) => throw new DocoptException(s"$pathTag doesn't exist: $path")
      case (Some(false), _, _, true) => throw new DocoptException(s"$pathTag already exists: $path")
      case (_, Some(true), None, true) if !path.isDirectory =>
        throw new DocoptException(s"Expected a directory, found file: $path")
      case (_, Some(true), Some(t), true) if !path.isDirectory =>
        throw new DocoptException(s"$t expected a directory, found file: $path")
      case (_, Some(false), None, true) if !path.isFile =>
        throw new DocoptException(s"Expected a file, found directory: $path")
      case (_, Some(false), Some(t), true) if !path.isFile =>
        throw new DocoptException(s"$t expected a file, found directory: $path")
      case _ => path
    }
  }

  def withTag(tag: String): PathValidator = copy(tag = Some(tag))
  def isPath: PathValidator = copy(ifIsDir = None)
  def isDir: PathValidator = copy(ifIsDir = Some(true))
  def isFile: PathValidator = copy(ifIsDir = Some(false))

  def exists(): PathValidator = copy(ifExists = Some(true))
  def doesntExist(): PathValidator = copy(ifExists = Some(false))
  def optionallyExists(): PathValidator = copy(ifExists = None)
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

      /** Get argument values as a String */
      override val string: DocoptGet[String] = (key: String) =>
        argMap.get(key) match {
          case Some(value: String)                     => Some(value)
          case Some(value: Iterable[String])           => Some(value.mkString(","))
          case Some(value: java.lang.Iterable[String]) => Some(value.asScala.mkString(","))
          case Some(value)                             => Some(value.toString)
          case None                                    => None
        }

      /** Get argument values as a String list */
      override val strings: DocoptGet[Iterable[String]] = (key: String) =>
        argMap.get(key) match {
          case Some(value: String)                     => Some(Seq(value))
          case Some(value: Iterable[String])           => Some(value)
          case Some(value: java.lang.Iterable[String]) => Some(value.asScala)
          case Some(value)                             => Some(Seq(value.toString))
          case None                                    => None
        }

      /** Get argument values as a String list */
      override val boolean: DocoptGet[Boolean] = (key: String) =>
        argMap.get(key) match {
          case Some(value: Iterable[String])           => Some(value.nonEmpty)
          case Some(value: java.lang.Iterable[String]) => Some(value.iterator().hasNext)
          case Some(value)                             => Some(value.toString.toBooleanOption.getOrElse(false))
          case None                                    => None
        }
    }
  }
}
