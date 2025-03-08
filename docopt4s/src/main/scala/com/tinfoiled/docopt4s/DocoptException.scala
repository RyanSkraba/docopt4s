package com.tinfoiled.docopt4s

/** Exception thrown when a docopt command line should cause the main application to abort (such as --help).
  *
  * @param msg
  *   the detail for the message returned by [[getMessage]]
  * @param ex
  *   if any, the cause to return from [[getCause]]
  * @param docopt
  *   the docopt string currently being used, or null if none
  * @param exitCode
  *   if any, the exit code to return when failing
  */
class DocoptException(
    msg: String,
    ex: Throwable = None.orNull,
    val docopt: String = None.orNull,
    val exitCode: Int = 0
) extends RuntimeException(msg, ex)
