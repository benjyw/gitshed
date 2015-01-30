package com.gitshed

import java.io.File

import scala.io.Source

import com.madgag.git._
import com.madgag.git.bfg.cleaner.ObjectIdCleaner
import org.eclipse.jgit.storage.file.FileRepositoryBuilder
import org.eclipse.jgit.internal.storage.file.FileRepository
import com.madgag.git.bfg.cleaner.protection.ProtectedObjectCensus


object Config {
  val defaultBinaryFileSuffixes = Set(
    // Image files.
    ".png", ".jpg", ".jpeg", ".gif", ".ico", ".tif", ".tiff", ".tga", ".bmp", ".pdf", ".ps", ".eps", ".raw",

    // Binary code and executables.
    ".pex", ".exe", ".class", ".o", ".so", ".pyc",

    // Archive files.
    ".jar", ".war", ".egg", ".zip", ".tar", ".gz", ".bz2",

    // Data files.
    ".bson", ".bin", ".dat"
  )

  // Read the list of suffixes that identify binary files, from a file (one per line).
  def readBinaryFileSuffixes(file: File): Set[String] = (Source.fromFile(file).getLines() map { _.trim() }).toSet

  def getConfig(args: Seq[String]): Option[Config] = {
    val parser = new scopt.OptionParser[Config]("gitshed-manage-history") {
      head("gitshed-manage-history", "0.1")
      opt[File]("repo-location").text("Act on the repo at this path.").action {
        (x, c) => c.copy(repoLocation = x)
      }
      opt[File]("binary-suffix-file").text("Path to file containing suffixes that identify binary files (one per line)").action {
        (x, c) => c.copy(binaryFileSuffixes=readBinaryFileSuffixes(x))
      }
      opt[Int]("other-binary-file-size-limit").text("The maximum allowed size of any file detected by git as binary, regardless of suffix").action {
        (x, c) => c.copy(otherBinaryTypeSizeLimitBytes=x)
      }
    }

    parser.parse(args, Config())
  }
}

case class Config(repoLocation: File=new File(System.getProperty("user.dir")),
                  binaryFileSuffixes: Set[String]=Config.defaultBinaryFileSuffixes,
                  otherBinaryTypeSizeLimitBytes: Int=8000)


object Main extends App {
  Config.getConfig(args) match {
    case Some(config) => BlobSymlinkingRepoRewriter.rewrite(config)
    case None => System.exit(1)  // Error will have been shown.
  }
}
