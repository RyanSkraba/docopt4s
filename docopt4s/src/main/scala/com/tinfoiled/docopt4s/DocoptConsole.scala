package com.tinfoiled.docopt4s

/** Create a console using the standard flags from the Docopt
  * @param opt
  *   The options keys and values that are currently set
  * @param verboseFlag
  *   The flag, if present, turns on the verbose mode.
  * @param noVerboseFlag
  *   The flag, if present, turns on the verbose mode.
  * @param plainFlag
  *   The flag, if present, turns on the plain mode.
  * @param yesFlag
  *   The flag, if present, turns on the yes mode.
  */
case class DocoptConsole(
    opt: Docopt,
    verboseFlag: String = "--verbose",
    noVerboseFlag: String = "--noVerbose",
    plainFlag: String = "--plain",
    yesFlag: String = "--yes"
) extends AnsiConsole {
  if (opt.flag(verboseFlag) && opt.flag(noVerboseFlag))
    throw new IncompatibleKeysException(verboseFlag, noVerboseFlag)

  override lazy val cfg: AnsiConsole.Cfg = AnsiConsole.Cfg(
    verbose = opt.flag(verboseFlag) || opt.boolean.getOption(noVerboseFlag).contains(false),
    plain = opt.flag(plainFlag),
    yes = opt.flag(yesFlag)
  )
}
