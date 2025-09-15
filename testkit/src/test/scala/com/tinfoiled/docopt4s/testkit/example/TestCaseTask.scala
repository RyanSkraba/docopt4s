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
       |  ${ExampleGo.Name} $Cmd --docopt DOC KEYS [ARGS...]
       |
       |Options:
       |  -h --help     Show this screen.
       |  --version     Show version.
       |  --docopt=DOC  If present, use as the default value for string arguments.
       |  KEYS          Comma separated list of keys to check in the results.
       |  ARGS          The arguments to pass to the docopt parser.
       |
       |Runs a test case and prints the values of the parsed options.
       |""".stripMargin.trim

  def go(opt: Docopt): Unit = {
    // TODO: Make docopt optional to read from STDIN
    val tcDoc = opt.string.getOption("--docopt").getOrElse(Source.fromInputStream(System.in, "UTF-8").mkString)
    // TODO: Make key separator ',' configurable
    val keyDelimiter = ','
    val tcKeys = opt.string.get("KEYS").split(keyDelimiter).filter(_.nonEmpty)
    // TODO: Get keys automatically from expected value provided as argument
    val tcArgs = opt.strings.get("ARGS")

    // Create a new Docopt from the arguments
    val tcOpts = Docopt(tcDoc, "0.0.0-ignored", tcArgs)

    // Print the values in a pseudo-JSON object with null for non-present keys, a single string value when there is
    // only one option value, and a list when there is more than one option value.
    val stringified = tcKeys.map(key =>
      key -> (tcOpts.strings.getOption(key) match {
        case None              => "null"
        case Some(Seq())       => "[]"
        case Some(Seq(single)) => s"\"$single\""
        case Some(multi)       => multi.mkString("[\"", "\",\"", "\"")
      })
    )

    val objectified = stringified.map { case k -> v => k + ":" + v }.mkString("{", ",", "}")
    print(objectified)
  }
}
