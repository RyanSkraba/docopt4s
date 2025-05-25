package com.tinfoiled.docopt4s

import scala.jdk.CollectionConverters._

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
}

object Docopt {

  def apply(doc: String, version: String, args: Iterable[String], optionsFirst: Boolean = false): Docopt = {
    try {
      // Delegate to the Java implementation
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

  def apply(opts: Map[String, AnyRef]): Docopt = {
    new Docopt {
      override def getStringsOption(key: String): Option[Iterable[String]] = opts.get(key) match {
        case Some(value: String)                     => Some(Seq(value))
        case Some(value: java.lang.Iterable[String]) => Some(value.asScala)
        case Some(value)                             => Some(Seq(value.toString))
        case None                                    => None
      }

      override def getStringOption(key: String): Option[String] = opts.get(key) match {
        case Some(value: String)                     => Some(value)
        case Some(value: java.lang.Iterable[String]) => Some(value.asScala.mkString(","))
        case Some(value)                             => Some(value.toString)
        case None                                    => None
      }

      override def getBooleanOption(key: String): Option[Boolean] =
        opts.get(key).map(_.toString.toBoolean)
    }
  }
}
