package com.tinfoiled.docopt4s

import org.docopt.Docopt

import scala.jdk.CollectionConverters._

/** A subcommand or task supported by an [[MultiTaskMain]] application. */
abstract class Task() {

  /** A helper class to extract options from the docopts.
    * @param opts
    *   The raw map exposed by docopts.
    */
  class TaskOptions(private val opts: java.util.Map[String, AnyRef]) {
    def getStringsOption(key: String): Option[Iterable[String]] = Option(opts.get(key)) match {
      case Some(value: String)                     => Some(Seq(value))
      case Some(value: java.lang.Iterable[String]) => Some(value.asScala)
      case Some(value)                             => Some(Seq(value.toString))
      case None                                    => None
    }
    def getStrings(key: String, default: Iterable[String]): Iterable[String] = getStringsOption(key).getOrElse(default)
    def getStrings(key: String): Iterable[String] = getStringsOption(key).getOrElse {
      throw new DocoptException(s"Expected $key not found")
    }

    def getStringOption(key: String): Option[String] = Option(opts.get(key)) match {
      case Some(value: String)                     => Some(value)
      case Some(value: java.lang.Iterable[String]) => Some(value.asScala.mkString(","))
      case Some(value)                             => Some(value.toString)
      case None                                    => None
    }
    def getString(key: String, default: String): String = getStringOption(key).getOrElse(default)
    def getString(key: String): String = getStringOption(key).getOrElse {
      throw new DocoptException(s"Expected $key not found")
    }

    def getIntOption(key: String): Option[Int] = getStringOption(key).map(value =>
      value.toIntOption.getOrElse { throw new DocoptException("Expected an integer for $key, but got $value") }
    )
    def getInt(key: String, default: Int): Int = getIntOption(key).getOrElse(default)
    def getInt(key: String): Int = getIntOption(key).getOrElse { throw new DocoptException(s"Expected $key not found") }

    def getBooleanOption(key: String): Option[Boolean] = Option(opts.get(key)).map(_.toString.toBoolean)
    def getBoolean(key: String, default: Boolean): Boolean = getBooleanOption(key).getOrElse(default)
    def getBoolean(key: String): Boolean = getBooleanOption(key).getOrElse {
      throw new DocoptException(s"Expected $key not found")
    }
  }

  /** The [[Docopt]] for the subcommand */
  val Doc: String

  /** The subcommand token, used to pick the task from the main application */
  val Cmd: String

  /** A short description for the subcommand */
  val Description: String

  /** Executes this subcommand
    * @param opts
    *   the docopts already parsed
    */
  def go(opts: TaskOptions): Unit
}
