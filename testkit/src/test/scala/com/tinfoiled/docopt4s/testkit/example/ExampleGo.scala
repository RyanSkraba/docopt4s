package com.tinfoiled.docopt4s.testkit.example

import com.tinfoiled.docopt4s.DocoptCliGo

/** A simple command line utility that collects two simple tasks. */
object ExampleGo extends DocoptCliGo {

  override lazy val Cli: String = "ExampleGo"

  override lazy val Version: String = "0.0.1-SNAPSHOT"

  override lazy val Tasks: Seq[DocoptCliGo.Task] = Seq(Example1Task, Example2Task)

  override lazy val Doc: String = "An example command line interface with multiple tasks.\n\n" + SimpleDoc

}
