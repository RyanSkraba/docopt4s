package com.tinfoiled.docopt4s

/** A task (or subcommand) supported by an [[MultiTaskMain]] application. */
abstract class Task {

  /** The [[Docopt]] string text for the task */
  val Doc: String

  /** The task token, used to pick the task from the main application */
  val Cmd: String = getClass.getSimpleName.replace('$', ' ').trim match {
    case tsk if tsk.endsWith("Task") => tsk.head.toLower +: tsk.tail.dropRight(4)
    case str                         => str.head.toLower +: str.tail
  }

  /** A short description for the task */
  val Description: String

  /** Executes this task
    * @param opt
    *   the docopt to fetch information from.
    */
  def go(opt: Docopt): Unit
}
