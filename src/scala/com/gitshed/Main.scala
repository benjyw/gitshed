package com.gitshed

import java.io.File

import com.madgag.git._
import com.madgag.git.bfg.cli.CLIConfig
import com.madgag.git.bfg.GitUtil.{tweakStaticJGitConfig, hasBeenProcessedByBFGBefore}
import com.madgag.git.bfg.cleaner.{ObjectIdCleaner, RepoRewriter}
import org.eclipse.jgit.storage.file.FileRepositoryBuilder
import org.eclipse.jgit.internal.storage.file.FileRepository
import com.madgag.git.bfg.cleaner.protection.ProtectedObjectCensus


object Main extends App {
  val repoLocation = new File("/tmp/rogue2") //new File(System.getProperty("user.dir"))
  val gitdir = resolveGitDirFor(repoLocation)
  val repo = FileRepositoryBuilder.create(gitdir.get).asInstanceOf[FileRepository]
  val objectIdCleanerConfig = ObjectIdCleaner.Config(ProtectedObjectCensus(Set("HEAD"))(repo))
  BlobSymlinkingRepoRewriter.rewrite(repo, objectIdCleanerConfig)

//  if (args.isEmpty) {
//    CLIConfig.parser.showUsage
//  } else {
//    CLIConfig.parser.parse(args, CLIConfig()) map {
//      config =>
//        tweakStaticJGitConfig(config.massiveNonFileObjects)
//        if (config.gitdir.isEmpty) {
//          CLIConfig.parser.showUsage
//          Console.err.println("Aborting : " + config.repoLocation + " is not a valid Git repository.\n")
//        } else {
//          implicit val repo = config.repo
//
//          println("\nUsing repo : " + repo.getDirectory.getAbsolutePath + "\n")
//
//          // do this before implicitly initiating big-blob search
//          if (hasBeenProcessedByBFGBefore(repo)) {
//            println("\nThis repo has been processed by The BFG before! Will prune repo before proceeding - to avoid unnecessary cleaning work on unused objects...")
//            repo.git.gc.call()
//            println("Completed prune of old objects - will now proceed with the main job!\n")
//          }
//
//          if (config.definesNoWork) {
//            Console.err.println("Please specify tasks for The BFG :")
//            CLIConfig.parser.showUsage
//          } else {
//            println("Found " + config.objectProtection.fixedObjectIds.size + " objects to protect")
//
//            RepoRewriter.rewrite(repo, config.objectIdCleanerConfig)
//          }
//        }
//    }
//  }

}