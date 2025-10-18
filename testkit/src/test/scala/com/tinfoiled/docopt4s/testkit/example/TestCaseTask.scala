package com.tinfoiled.docopt4s.testkit.example

import com.tinfoiled.docopt4s.{Docopt, Task}

import scala.io.Source

/** The test case can be used to parse arguments given a dynamically provided [[Docopt]] text. */
object TestCaseTask extends Task {

  val Cmd = "test"

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
       |""".stripMargin.trim

  def go(opt: Docopt): Unit = {

    // Create the docopt from the spec and the arguments
    val tcDoc = opt.string.getOption("--docopt").getOrElse(Source.fromInputStream(System.in, "UTF-8").mkString)
    val tcArgs = opt.strings.get("ARGS")
    val tcOpt = Docopt(tcDoc, "0.0.0-ignored", tcArgs)

    // Print the values in a pseudo-JSON object with null for non-present keys, a single string value when there is
    // only one option value, and a list when there is more than one option value.
    def stringify(keys: Seq[String]): String = {
      keys
        .filter(_.nonEmpty)
        .map(key =>
          key -> (tcOpt.strings.getOption(key).map(_.toSeq) match {
            case None              => "null"
            case Some(Seq())       => "[]"
            case Some(Seq(single)) => s"\"$single\""
            case Some(multi)       => multi.mkString("[\"", "\",\"", "\"]")
          })
        )
        .filter(_._2 != "null")
        .map { case k -> v => "\"" + k + "\":" + v }
        .mkString("{", ",", "}")
    }

    // if --keys is specified then extract and print all the keys
    opt.string.getOption("--keys").foreach { tcKeys =>
      // TODO: Make key separator ',' configurable
      val keyDelimiter = ','
      print(stringify(tcKeys.split(keyDelimiter)))
    }

    // if --check is specified then verify that the expected result is returned
    opt.string.getOption("--check").foreach { expected =>
      val expectedKeys = raw""""([^"]*)":""".r.findAllMatchIn(expected).map(_.group(1).trim).toSeq
      val actual = stringify(expectedKeys)
      if (actual == expected) print("OK")
      else Console.err.print(s"NOK: $actual != $expected")
    }
  }
}
