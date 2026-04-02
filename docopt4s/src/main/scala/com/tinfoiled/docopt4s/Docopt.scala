package com.tinfoiled.docopt4s

import java.nio.file.{Files, Path, Paths}

import scala.jdk.CollectionConverters._
import scala.util.Properties

/** A [[Docopt]] provides a means to interpret command line arguments via a help text.
  *
  * An "argument" refers to strings passed to the application from the command line. The Docopt specification describes
  * how arguments can be parsed into "options" by associating them with a key (taken from the Docopt help string) and
  * returning a value.
  *
  * Frequently an option key corresponds to an argument "--keyname" and the following argument is the option value. For
  * positional options, the key isn't an argument but the option value is.
  *
  * Options without a option value argument only rely on whether the option key is present, and is called a "flag".
  *
  * The Docopt text spec is described at https://docopt.org/.
  */
trait Docopt {

  /** Get option values as a {{String}} */
  val string: DocoptGetNoVld[String]

  /** Get option values as an {{Iterable}} String list */
  val strings: DocoptGetStrings

  /** Get option values as a {{Boolean}} */
  val boolean: DocoptGetNoVld[Boolean]

  /** @param key
    *   The option key
    * @return
    *   True only if the option key is present (and the value evaluates to true) else false.
    */
  def flag(key: String): Boolean = boolean.getOr(key, false)

  /** Get option values as an {{Int}} */
  val int: DocoptGetNoVld[Int] = new DocoptGetNoVld[Int](key =>
    string
      .getOption(key)
      .map(value =>
        value.toIntOption.getOrElse {
          throw new DocoptException(s"Expected an integer for $key, but got $value")
        }
      )
  )

  /** Get option values as a filesystem {{Path}} */
  val path: DocoptGet[Path, PathValidator] = new DocoptGet[Path, PathValidator](PathValidator().isPath) {
    override def getOption(key: String, vld: PathValidator): Option[Path] =
      string.getOption(key).map(_ => vld.validate(string.get(key)))
  }

  /** Get option values as a filesystem path validated as a regular file */
  val file: DocoptGet[Path, PathValidator] = new DocoptGet[Path, PathValidator](PathValidator().isFile) {
    override def getOption(key: String, vld: PathValidator): Option[Path] =
      path.getOption(key, vld.isFile)
  }

  /** Get option values as a filesystem path validated as a directory */
  val dir: DocoptGet[Path, PathValidator] = new DocoptGet[Path, PathValidator](PathValidator().isDir) {
    override def getOption(key: String, vld: PathValidator): Option[Path] =
      path.getOption(key, vld.isDir)
  }
}

/** Given the option key, finds the option value converted to the expected type. The different methods determine how
  * missing option keys are treated: as an error, using a default or as a Scala {{Option}}.
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
    *   If the key is present and can be transformed to the type, return the transformed option value or {{None}} if not
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
    throw new DocoptException(s"Required $key not found")
  }
}

/** A [[DocoptGet]] that doesn't require or use a validator.
  * @param getter
  *   A helper method to get the option value.
  * @tparam T
  *   The expected type of the command line argument.
  */
class DocoptGetNoVld[T](getter: String => Option[T]) extends DocoptGet[T, Option[Nothing]](None) {
  override def getOption(key: String, vld: Option[Nothing]): Option[T] = getter(key)
}

class DocoptGetStrings(getter: String => Option[Iterable[String]]) extends DocoptGetNoVld[Iterable[String]](getter) {

  /** Shortcut to get the option value or an empty list if it isn't present.
    * @param key
    *   The option key
    * @return
    *   The transformed option value or an empty list if not present.
    */
  def getOrEmpty(key: String): Iterable[String] = getOr(key, Seq.empty)
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
    val path = Paths
      .get(
        root
          .map(_.toString)
          .orElse(systemEnvVar.flatMap(sys.env.get(_)))
          .orElse(Option(Properties.userDir))
          .getOrElse("/")
      )
      .resolve(value)
      .toAbsolutePath
      .normalize()

    if (Files.exists(path)) {
      // If the path mustn't exist but it does
      if (ifExists.contains(false))
        throw new DocoptException(
          tag match {
            case Some(t)                        => s"$t already exists: $path"
            case _ if Files.isRegularFile(path) => s"File already exists: $path"
            case _                              => s"Directory already exists: $path"
          }
        )

      // if the path should be a directory, but it isn't
      if (ifIsDir.contains(true) && !Files.isDirectory(path))
        throw new DocoptException(
          tag match {
            case Some(t) => s"$t expected a directory, found file: $path"
            case _       => s"Expected a directory, found file: $path"
          }
        )

      // if the path should be a file, but it isn't
      if (ifIsDir.contains(false) && !Files.isRegularFile(path))
        throw new DocoptException(
          tag match {
            case Some(t) => s"$t expected a file, found directory: $path"
            case _       => s"Expected a file, found directory: $path"
          }
        )
    } else {
      // If the path must exist but it doesn't
      if (ifExists.contains(true))
        throw new DocoptException(s"$pathTag doesn't exist: $path")

      // If it mustn't exist, and it doesn't exist, but it's existing parent is File, then
      // the path is uncreatable and an error.
      if (ifExists.contains(false)) {
        val existingParent = LazyList.iterate(path)(_.getParent).takeWhile(_ != null).find(Files.exists(_))
        if (existingParent.exists(Files.isRegularFile(_)))
          throw new DocoptException(s"$pathTag is uncreatable, ${existingParent.get} exists: $path")
      }
    }

    path
  }

  def withTag(tag: String): PathValidator = copy(tag = Some(tag))
  def withRoot(root: String): PathValidator = copy(root = Some(root))
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

      override val string: DocoptGetNoVld[String] = new DocoptGetNoVld[String](argMap.get(_) match {
        case Some(value: String)                => Some(value)
        case Some(value: Iterable[_])           => Some(value.mkString(","))
        case Some(value: java.lang.Iterable[_]) => Some(value.asScala.mkString(","))
        case Some(value)                        => Some(value.toString)
        case None                               => None
      })

      override val strings: DocoptGetStrings =
        new DocoptGetStrings(argMap.get(_) match {
          case Some(value: String)                => Some(Seq(value))
          case Some(value: Iterable[_])           => Some(value.map(_.toString))
          case Some(value: java.lang.Iterable[_]) => Some(value.asScala.map(_.toString))
          case Some(value)                        => Some(Seq(value.toString))
          case None                               => None
        })

      override val boolean: DocoptGetNoVld[Boolean] = new DocoptGetNoVld[Boolean](argMap.get(_) match {
        case Some(value: Iterable[_])           => Some(value.mkString(",").toBooleanOption.getOrElse(false))
        case Some(value: java.lang.Iterable[_]) => Some(value.asScala.mkString(",").toBooleanOption.getOrElse(false))
        case Some(value)                        => Some(value.toString.toBooleanOption.getOrElse(false))
        case None                               => None
      })
    }
  }
}
