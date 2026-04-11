package com.tinfoiled.docopt4s.testkit.example

import com.tinfoiled.docopt4s.{Docopt, Task}

import scala.io.Source
import scala.util.{Failure, Success, Try}

/** The test case can be used to parse arguments given a dynamically provided [[Docopt]] text. */
object TestCaseTask extends Task {

  override val Cmd = "test"

  val Description = "Run a test case."

  val Doc: String =
    s"""$Description
       |
       |Usage:
       |  ${ExampleGo.Name} $Cmd --docopt DOC --keys KEYS [ARGS...]
       |  ${ExampleGo.Name} $Cmd --docopt DOC --check CHECK [ARGS...]
       |
       |Options:
       |  -h --help      Show this screen.
       |  --version      Show version.
       |  --docopt=DOC   If present, use as the default value for string arguments.
       |  --keys=KEYS    Comma separated list of keys to retrieve from the results.
       |  --check=CHECK  A JSON-like representation of the expected keys.
       |  ARGS           The arguments to pass to the docopt parser.
       |
       |Runs a test case and prints the values of the parsed options.  When the --keys
       |option is used, the output is the values in the keys as a JSON object (where
       |all keys are present and all values are either a string or list of strings).
       |
       |Examples:
       |
       |(TODO: the X should be removed in UsageX: for this to actually work)
       |
       |  ${ExampleGo.Name} $Cmd --docopt "UsageX: program [--flag] CMD ARGS..." \\
       |      --keys "--flag,CMD,ARGS" a1 a2 a3 a4 a5
       |
       |  {"--flag":"false","CMD":"a1","ARGS:["a2","a3","a4","a5"]}
       |
       |  ${ExampleGo.Name} $Cmd --docopt "UsageX: program [--flag] CMD ARGS..." \\
       |      --check $$'{"--flag":"false","CMD":"a1","ARGS:["a2","a3","a4","a5"]}'
       |
       |""".stripMargin.trim

  def go(opt: Docopt): Unit = {

    // Create the docopt from the spec and the arguments
    val tcDoc = opt.string.getOption("--docopt").getOrElse(Source.fromInputStream(System.in, "UTF-8").mkString)
    val tcArgs = opt.strings.get("ARGS")

    // if --keys is specified then extract and print all the keys
    opt.string.getOption("--keys").foreach { tcKeys =>
      // TODO: Make key separator ',' configurable
      val keyDelimiter = ','
      print(jsonifyKeys(Docopt(tcDoc, "0.0.0-ignored", tcArgs), tcKeys.split(keyDelimiter).toSeq))
    }

    // if --check is specified then verify that the expected result is returned
    opt.string.getOption("--check").foreach { expected =>
      checkTest(tcDoc, tcArgs, expected) match {
        case Success(_)  => print("OK")
        case Failure(ex) => Console.err.print(s"NOK: ${ex.getMessage} != $expected")
      }
    }
  }

  /** Performs a test on a docopt string with given arguments, and an expected result.
    *
    * @param docopt
    *   The docopt specification under test
    * @param args
    *   The arguments to apply to the specification
    * @param expected
    *   The open keys and values to test as a result
    * @return
    *   A success if the expected values were returned, and a failure with the actual values discovered as the exception
    *   message otherwise.
    */
  def checkTest(docopt: String, args: Iterable[String], expected: String): Try[String] = {
    val expectedKeys = raw""""([^"]*)":""".r.findAllMatchIn(expected).map(_.group(1).trim).toSeq
    val actual = jsonifyKeys(Docopt(docopt, "0.0.0-ignored", args), expectedKeys)
    if (actual == expected) Success(expected)
    else Failure(new Exception(actual))
  }

  /** Given option keys, fetches option values from the docopt in a pseudo-JSON object: null for non-present keys, a
    * single string value when there is only one option value, and a string list when there is more than one option
    * value.
    *
    * @param opt
    *   The Docopt instance that has been parsed from the arguments
    * @param keys
    *   The option keys to fetch
    * @return
    *   A pseudo-JSON that shows the option values corresponding to the keys
    */
  def jsonifyKeys(opt: Docopt, keys: Seq[String]): String = {
    keys
      .filter(_.nonEmpty)
      .map(key =>
        key -> (opt.strings.getOption(key).map(_.toSeq) match {
          case None              => "null"
          case Some(Seq())       => "[]"
          case Some(Seq(single)) => "\"" + single.replace("(\\\")", "\\$1") + "\""
          case Some(multi)       => multi.map(_.replace("(\\\")", "\\$1")).mkString("[\"", "\",\"", "\"]")
        })
      )
      .filter(_._2 != "null")
      .map { case k -> v => "\"" + k + "\":" + v }
      .mkString("{", ",", "}")
  }
}
