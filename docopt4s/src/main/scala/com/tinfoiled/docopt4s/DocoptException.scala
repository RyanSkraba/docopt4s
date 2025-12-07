package com.tinfoiled.docopt4s

/** Exception thrown when a docopt command line should cause the main application to abort (such as --help), or when an
  * option value can't be validated as an expected type.
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
    val exitCode: Int = 1
) extends RuntimeException(msg, ex)

// TODO:
// /** Key required but not provided */
// class RequiredOptionKeyException(
//    key: String,
//    override val docopt: String,
// ) extends DocoptException(s"Required $key not present", null, docopt, 1)

// TODO:
// /** Value required for a key but not provided */
// class RequiredOptionValueException(
//    key: String,
//    override val docopt: String,
// ) extends DocoptException(s"Required $key not present", null, docopt, 1)
