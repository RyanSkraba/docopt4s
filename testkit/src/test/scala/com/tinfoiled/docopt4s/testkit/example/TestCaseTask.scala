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
       |  {"--flag":false,"CMD":"a1","ARGS:["a2","a3","a4","a5"]}
       |
       |  ${ExampleGo.Name} $Cmd --docopt "UsageX: program [--flag] CMD ARGS..." \\
       |      --check $$'{"--flag":false,"CMD":"a1","ARGS:["a2","a3","a4","a5"]}'
       |
       |""".stripMargin.trim

  def go(opt: Docopt): Unit = {

    // if --keys is specified then extract and print all the keys
    opt.string.getOption("--keys").foreach { tcKeys =>
      val tcDoc = opt.string.getOption("--docopt").getOrElse(Source.fromInputStream(System.in, "UTF-8").mkString)
      val tcArgs = opt.strings.get("ARGS")
      // TODO: Make key separator ',' configurable
      val keyDelimiter = ','
      print(TestCase.jsonifyKeys(Docopt(tcDoc, "0.0.0-ignored", tcArgs), tcKeys.split(keyDelimiter).toSeq))
    }

    // if --check is specified then verify that the expected result is returned
    opt.string.getOption("--check").foreach { expected =>
      val tcDoc = opt.string.getOption("--docopt").getOrElse(Source.fromInputStream(System.in, "UTF-8").mkString)
      val tcArgs = opt.strings.get("ARGS")
      TestCase(tcDoc, ujson.read(expected).obj, tcArgs).execute() match {
        case Success(_)                  => print("OK")
        case Failure(e: DocoptException) => throw e;
        case Failure(ex)                 => Console.err.print(s"NOK: ${ex.getMessage} != $expected")
      }
    }

    // if --file is specified then read it from the system
    opt.file.getOption("--file").foreach { file =>
      val tests = TestCase.parse(Using.resource(Source.fromFile(file.toFile)) { _.getLines().mkString("\n") })
      for ((tc, _) <- tests.zipWithIndex) {
        tc.execute().get
      }
    }
  }

  /** A test case that can be run.
    * @param docopt
    *   The docopt specification under test
    * @param args
    *   The arguments to apply to the specification
    * @param expected
    *   The open keys and values to test as a result
    */
  case class TestCase(docopt: String, expected: ujson.Value, args: Iterable[String]) {

    /** Performs a test on a docopt string with given arguments, and an expected result.
      *
      * @return
      *   A success if the expected values were returned, and a failure with the actual values discovered as the
      *   exception message otherwise.
      */
    def execute(): Try[Unit] = {
      Try(Docopt(docopt, "0.0.0-ignored", args)) match {
        case Success(docopt) =>
          val expectedKeys = expected.obj.value.keys.toSeq
          Try {
            val actual = TestCase.jsonifyKeys(docopt, expectedKeys)
            if (actual != expected) throw new AssertionError(actual)
          }
        case Failure(ex: DocoptException) if expected == ujson.Str("user-error") =>
          // TODO: improve this
          Success(())
        case Failure(ex) =>
          Failure(ex)
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

    def parse(input: String): Seq[TestCase] =
      TestCaseRegex
        .findAllMatchIn(input)
        .flatMap { m =>
          val docopt = m.group(1)
          val rest = m.group(2)
          rest.linesIterator
            .map(_.trim)
            .filter(_.nonEmpty) // Remove blank lines
            .filter(!_.startsWith("#")) // Remove comments
            .grouped(2)
            .map(_.toSeq)
            .flatMap {
              case Seq(args, expected) if args.startsWith("$") =>
                Some(TestCase(docopt, ujson.read(expected), splitArgs(args.substring(1).trim).tail))
              case Seq(args, expected) => Some(TestCase(docopt, ujson.read(expected), splitArgs(args)))
              case _                   => None
            }
        }
        .toSeq

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

    /** Given option keys, fetches option values from the docopt in a JSON object: null for non-present keys, a single
      * string value when there is only one option value, and a string list when there is more than one option value.
      * The literal strings "true" and "false" are converted to boolean (but only lower case).
      *
      * @param opt
      *   The Docopt instance that has been parsed from the arguments
      * @param keys
      *   The option keys to fetch
      * @return
      *   A pseudo-JSON that shows the option values corresponding to the keys
      */
    def jsonifyKeys(opt: Docopt, keys: Seq[String]): ujson.Obj =
      ujson.Obj
        .from(
          keys
            .filter(_.nonEmpty)
            .flatMap(key =>
              opt.strings.getOption(key).map(_.toSeq) match {
                case None               => None
                case Some(Seq())        => Some(key -> ujson.Arr())
                case Some(Seq("true"))  => Some(key -> ujson.True)
                case Some(Seq("false")) => Some(key -> ujson.False)
                case Some(Seq(single))  => Some(key -> ujson.Str(single))
                case Some(multi)        => Some(key -> ujson.Arr.from(multi))
              }
            )
        )
  }
}
