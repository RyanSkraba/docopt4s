package com.tinfoiled.docopt4s

/** A task (or subcommand) supported by an [[MultiTaskMain]] application. */
abstract class Task() {

  /** The [[Docopt]] string text for the task */
  val Doc: String

  /** The task token, used to pick the task from the main application */
  val Cmd: String

  /** A short description for the task */
  val Description: String

  /** Executes this task
    * @param opt
    *   the docopt to fetch information from.
    */
  def go(opt: Docopt): Unit
}
