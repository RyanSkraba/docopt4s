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
    def getStringsOption(key: String): Option[Iterable[String]] =
      Option(opts.get(key)).map(_.asInstanceOf[java.lang.Iterable[String]].asScala)
    def getStrings(key: String, default: Iterable[String]): Iterable[String] =
      getStringsOption(key).getOrElse(default)
    def getStrings(key: String): Iterable[String] = getStringsOption(key).get

    def getStringOption(key: String): Option[String] = Option(opts.get(key)).map(_.toString)
    def getString(key: String, default: String): String = getStringOption(key).getOrElse(default)
    def getString(key: String): String = getStringOption(key).get

    def getIntOption(key: String): Option[Int] = Option(opts.get(key)).map(_.toString.toInt)
    def getInt(key: String, default: Int): Int = getIntOption(key).getOrElse(default)
    def getInt(key: String): Int = getIntOption(key).get

    def getBooleanOption(key: String): Option[Boolean] = Option(opts.get(key)).map(_.toString.toBoolean)
    def getBoolean(key: String, default: Boolean): Boolean = getBooleanOption(key).getOrElse(default)
    def getBoolean(key: String): Boolean = getBooleanOption(key).get
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
