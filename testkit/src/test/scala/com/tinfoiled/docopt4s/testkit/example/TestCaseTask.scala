package com.tinfoiled.docopt4s.testkit.example

import com.tinfoiled.docopt4s.{Docopt, DocoptException, Task}

import scala.io.Source
import scala.util.{Failure, Success, Try, Using}

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
       |  ${ExampleGo.Name} $Cmd --file FILE
       |
       |Options:
       |  -h --help      Show this screen.
       |  --version      Show version.
       |  --docopt=DOC   If present, use as the default value for string arguments.
       |  --keys=KEYS    Comma separated list of keys to retrieve from the results.
       |  --check=CHECK  A JSON-like representation of the expected keys.
       |  --file=FILE    A file of test cases to read and execute.
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
      TestCase(tcDoc, expected, tcArgs).execute() match {
        case Success(_)                  => print("OK")
        case Failure(e: DocoptException) => throw e;
        case Failure(ex)                 => Console.err.print(s"NOK: ${ex.getMessage} != $expected")
      }
    }

    // if --file is specified then read it from the system
    opt.file.getOption("--file").foreach { file =>
      val contents = Using.resource(Source.fromFile(file.toFile)) { _.getLines().mkString }
      println(contents)
    }
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

  /** A test case that can be run.
    * @param docopt
    *   The docopt specification under test
    * @param args
    *   The arguments to apply to the specification
    * @param expected
    *   The open keys and values to test as a result
    */
  case class TestCase(docopt: String, expected: String, args: Iterable[String]) {

    /** Performs a test on a docopt string with given arguments, and an expected result.
      *
      * @return
      *   A success if the expected values were returned, and a failure with the actual values discovered as the
      *   exception message otherwise.
      */
    def execute(): Try[String] = {
      val expectedKeys = raw""""([^"]*)":""".r.findAllMatchIn(expected).map(_.group(1).trim).toSeq
      Try {
        val actual = jsonifyKeys(Docopt(docopt, "0.0.0-ignored", args), expectedKeys)
        if (actual != expected) throw new AssertionError(actual)
        expected
      }
    }
  }

  object TestCase {
    private[this] val TestCaseRegex = {
      val q3 = "\"\"\""
      raw"""(?sx)
          (?<=\n|^)r?$q3
          (.*?)
          $q3\s*\n
          (.*?)(?=(\nr?$q3|$$))
          """.r
    }

    def apply(docopt: String, expected: String, arg0: String, args: String*): TestCase =
      TestCase(docopt, expected, arg0 +: args)

    def parse(input: String): Seq[TestCase] =
      TestCaseRegex
        .findAllMatchIn(input)
        .flatMap { m =>
          val docopt = m.group(1)
          val rest = m.group(2)
          rest.linesIterator
            .map(_.trim)
            .filter(_.nonEmpty)
            .grouped(2)
            .map(_.toSeq)
            .flatMap {
              case Seq(args, expected) if args.startsWith("$") =>
                Some(TestCase(docopt, expected, splitArgs(args.substring(1).trim).tail))
              case Seq(args, expected) => Some(TestCase(docopt, expected, splitArgs(args)))
              case _                   => None
            }
        }
        .toSeq
  }

  private[this] val RegexQuotedArgs = raw""""(?:[^"\\]|\\.)*"|\S+""".r

  /** Tokenizes the input string into arguments, separated by whitespace. Only double quotes are recognized if
    * whitespace is important to the argument, and internal quotes must be escaped.
    * @param input
    *   A string of input to tokenize by whitespace
    * @return
    *   The list of arguments from the line
    */
  def splitArgs(input: String): Iterable[String] =
    RegexQuotedArgs
      .findAllMatchIn(input)
      .map { _.matched }
      .map {
        case s if s.startsWith("\"") && s.endsWith("\"") => s.drop(1).dropRight(1)
        case s                                           => s
      }
      .map { _.replaceAll(raw"\\(.)", "$1") }
      .to(Iterable)
}
