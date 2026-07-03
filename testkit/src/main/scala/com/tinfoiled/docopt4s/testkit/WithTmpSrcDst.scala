package com.tinfoiled.docopt4s.testkit

import com.tinfoiled.docopt4s.FsPath.RichPath

import java.nio.file.Path

/** Trait for including file system helpers. */
trait WithTmpSrcDst extends TmpDir { this: MultiTaskMainSpec[_] =>

  /** Creates a scenario in the temporary directory with some files and directories in it
    *
    * @param tag
    *   A string tag to use to uniquely identify the scenario
    * @param srcPaths
    *   Files and directories relative to the source directory to create. Directories end with a slash, files are filled
    *   with the text of their filename.
    * @return
    *   A source directory potentially populated with files, and an empty destination directory.
    */
  def createSrcDst(tag: String, srcPaths: String*): (Path, Path) = {
    val src = (Tmp / tag / "src").createDirectory(failIfExists = false)
    val dst = (Tmp / tag / "dst").createDirectory(failIfExists = false)
    for (p <- srcPaths) {
      val srcPath = src / p
      if (p.endsWith("/"))
        srcPath.createDirectory(failIfExists = false)
      else {
        srcPath.getParent.createDirectory(failIfExists = false)
        srcPath.createFile().writeAll(srcPath.name())
      }
    }
    (src, dst)
  }

  /** A helper method for running a MultiTaskMain with an assumed successful result.
    * @param replacements
    *   A list of pairs of strings to replace in the output.
    * @param args
    *   The arguments to apply to the ammonite script
    * @return
    *   The output of the script with all of the string replacements applied, as well as replacing the temporary
    *   directory with &lt;TMP&gt;.
    */
  def withGoStdoutReplace(replacements: (Any, String)*)(args: Any*): String = {
    val stdout = withGoStdout(args: _*)
    replacements
      .foldLeft(stdout) { (acc, r) => acc.replace(r._1.toString, r._2) }
      .replace(Tmp.toString, "<TMP>")
  }

  /** A helper method for running a MultiTaskMain with an assumed successful result, specifically for scenarios
    * involving source and destination directories.
    *
    * @param src
    *   The source directory.
    * @param dst
    *   The destination directory.
    * @param replacements
    *   A list of pairs of strings to replace in the output.
    * @param args
    *   The arguments to apply to the ammonite script.
    * @return
    *   The output of the script with all of the string replacements applied, as well as replacing the source and
    *   destination directories with &lt;SRC&gt; and &lt;DST&gt; respectively.
    */
  def withGoStdoutSrcDst(src: Path, dst: Path, replacements: (Any, String)*)(args: Any*): String =
    withGoStdoutReplace(replacements ++ Seq(src.toString -> "<SRC>", dst.toString -> "<DST>"): _*)(args: _*)
}
