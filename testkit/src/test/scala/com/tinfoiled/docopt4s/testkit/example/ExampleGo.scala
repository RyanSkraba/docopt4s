package com.tinfoiled.docopt4s.testkit.example

import com.tinfoiled.docopt4s.{MultiTaskMain, Task}

/** A simple command line utility that collects two simple tasks. */
object ExampleGo extends MultiTaskMain {

  override lazy val Name: String = "ExampleGo"

  override lazy val Version: String = "0.0.1-SNAPSHOT"

  override lazy val Tasks: Seq[Task] = Seq(Example1Task, Example2Task)

  override lazy val Doc: String = "An example command line interface with multiple tasks.\n\n" + SimpleDoc

}
