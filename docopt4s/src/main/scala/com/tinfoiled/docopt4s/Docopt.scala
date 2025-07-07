package com.tinfoiled.docopt4s

import scala.jdk.CollectionConverters._
import scala.reflect.io.{Directory, File, Path}
import scala.util.Properties

/** A [[Docopt]] provides a means to interpret command line arguments via a help text.
  *
  * An "argument" refers to strings passed to the application from the command line. The Docopt specification describes
  * how arguments can be parsed into "options" by associating them with a key (taken from the Docopt help string) and
  * returning a value.
  *
  * The Docopt text spec is described at https://docopt.org/.
  */
trait Docopt {

  /** Get option values as a [[String]] */
  val string: DocoptGet[String, Null]

  /** Get option values as an [[Iterable]] String list */
  val strings: DocoptGet[Iterable[String], Null]

  /** Get option values as a [[Boolean]] */
  val boolean: DocoptGet[Boolean, Null]

  /** Get option values as an [[Int]] */
  val int: DocoptGet[Int, Null] = new DocoptGet[Int, Null](null) {
    override def getOption(key: String, vld: Null): Option[Int] = string
      .getOption(key)
      .map(value =>
        value.toIntOption.getOrElse {
          throw new DocoptException(s"Expected an integer for $key, but got $value")
        }
      )
  }

  /** Get option values as a [[Path]] */
  val path: DocoptGet[Path, PathValidator] = new DocoptGet[Path, PathValidator](PathValidator().isPath) {
    override def getOption(key: String, vld: PathValidator): Option[Path] =
      string.getOption(key).map(_ => vld.validate(string.get(key)))
  }

  /** Get option values as a [[File]] */
  val file: DocoptGet[File, PathValidator] = new DocoptGet[File, PathValidator](PathValidator().isFile) {
    override def getOption(key: String, vld: PathValidator): Option[File] =
      path.getOption(key, vld.isFile).map(_.toFile)
  }

  /** Get option values as a [[Directory]] */
  val dir: DocoptGet[Directory, PathValidator] = new DocoptGet[Directory, PathValidator](PathValidator().isDir) {
    override def getOption(key: String, vld: PathValidator): Option[Directory] =
      path.getOption(key, vld.isDir).map(_.toDirectory)
  }
}

/** Given the option key, finds the option value converted to the expected type. The different methods determine how
  * missing option keys are treated: as an error, using a default or as a Scala [[Option]].
  *
  * @tparam T
  *   The expected type of the command line argument.
  * @tparam VLD
  *   A helper validator type.
  */
abstract class DocoptGet[T, VLD](DefaultVld: VLD) {

  /** @param key
    *   The option key
    * @param vld
    *   The validator used to check whether the path value exists, and/or is of the right type.
    * @return
    *   If the key is present and can be transformed to the type, return the transformed option value or [[None]] if not
    *   present.
    * @throws DocoptException
    *   If it is present but can't be converted or is invalid.
    */
  @throws[DocoptException]
  def getOption(key: String, vld: VLD = DefaultVld): Option[T]

  /** @param key
    *   The option key
    * @param vld
    *   The validator used to check whether the path value exists, and/or is of the right type.
    * @param default
    *   The default option value to use if the key is not present.
    * @return
    *   If the key is present and can be transformed to the type, return the transformed option value or use the default
    *   if not present.
    * @throws DocoptException
    *   If it is present but can't be converted or is invalid.
    */
  @throws[DocoptException]
  def getOr(key: String, default: T, vld: VLD = DefaultVld): T = getOption(key, vld).getOrElse(default)

  /** @param key
    *   The option key
    * @param vld
    *   The validator used to check whether the path value exists, and/or is of the right type.
    * @return
    *   If the key present and can be transformed to the type, return the transformed argument value.
    * @throws DocoptException
    *   If it is not present, or present but can't be converted or is invalid.
    */
  @throws[DocoptException]
  def get(key: String, vld: VLD = DefaultVld): T = getOption(key, vld).getOrElse {
    throw new DocoptException(s"Expected $key not found")
  }
}

/** Helper to validate option values against an expected filesystem state.
  *
  * @param root
  *   An absolute directory to use in constructing the path
  * @param tag
  *   A human-readable description to describe the option, used in exceptions.
  * @param ifIsDir
  *   Whether to test to ensure the option value must be a Directory or must be a File (or None if it doesn't matter).
  * @param ifExists
  *   Whether to test to ensure the option value must exist or must not exist (or None if it doesn't matter).
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
    *   An option value (as a string) to convert and test as a Path, File or Directory
    * @return
    *   The absolute, validated path that the option value represents on the filesystem.
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
      // Delegate to the Java implementation to obtain a map of option keys and values
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

      override val string: DocoptGet[String, Null] = new DocoptGet[String, Null](null) {
        override def getOption(key: String, vld: Null): Option[String] = argMap.get(key) match {
          case Some(value: String)                     => Some(value)
          case Some(value: Iterable[String])           => Some(value.mkString(","))
          case Some(value: java.lang.Iterable[String]) => Some(value.asScala.mkString(","))
          case Some(value)                             => Some(value.toString)
          case None                                    => None
        }
      }

      override val strings: DocoptGet[Iterable[String], Null] = new DocoptGet[Iterable[String], Null](null) {
        override def getOption(key: String, vld: Null): Option[Iterable[String]] = argMap.get(key) match {
          case Some(value: String)                     => Some(Seq(value))
          case Some(value: Iterable[String])           => Some(value)
          case Some(value: java.lang.Iterable[String]) => Some(value.asScala)
          case Some(value)                             => Some(Seq(value.toString))
          case None                                    => None
        }
      }

      override val boolean: DocoptGet[Boolean, Null] = new DocoptGet[Boolean, Null](null) {
        override def getOption(key: String, vld: Null): Option[Boolean] = argMap.get(key) match {
          case Some(value: Iterable[String])           => Some(value.nonEmpty)
          case Some(value: java.lang.Iterable[String]) => Some(value.iterator().hasNext)
          case Some(value)                             => Some(value.toString.toBooleanOption.getOrElse(false))
          case None                                    => None
        }
      }
    }
  }
}
