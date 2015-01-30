package com.gitshed

import org.eclipse.jgit.transport.ReceiveCommand
import org.eclipse.jgit.revwalk.RevSort._
import com.madgag.git.bfg.Timing
import scala.concurrent.Future
import concurrent.ExecutionContext.Implicits.global
import scala.collection.convert.wrapAll._
import com.madgag.git._
import org.eclipse.jgit.lib.ObjectId
import org.eclipse.jgit.revwalk.{RevWalk, RevCommit}
import com.madgag.git.bfg.cleaner.{CLIReporter, Reporter, ObjectIdCleaner}
import org.eclipse.jgit.storage.file.FileRepositoryBuilder
import org.eclipse.jgit.internal.storage.file.FileRepository
import com.madgag.git.bfg.cleaner.protection.ProtectedObjectCensus


// Copied and modified from com.madgag.git.bfg.cleaner.RepoRewriter.
object BlobSymlinkingRepoRewriter {

  def rewrite(config: Config): Map[ObjectId, ObjectId] = {
    val gitdir = resolveGitDirFor(config.repoLocation)
    val repo = FileRepositoryBuilder.create(gitdir.get).asInstanceOf[FileRepository]
    assert(!repo.getAllRefs.isEmpty, "Can't find any refs in repo at " + repo.getDirectory.getAbsolutePath)

    val objectIdCleanerConfig = ObjectIdCleaner.Config(ProtectedObjectCensus(Set("HEAD"))(repo))

    implicit val refDatabase = repo.getRefDatabase

    val reporter: Reporter = new CLIReporter(repo)
    implicit val progressMonitor = reporter.progressMonitor

    val allRefs = repo.getAllRefs.values

    def createRevWalk: RevWalk = {
      val revWalk = new RevWalk(repo)
      revWalk.sort(TOPO) // crucial to ensure we visit parents BEFORE children, otherwise blow stack
      revWalk.sort(REVERSE, true) // we want to start with the earliest commits and work our way up...
      val startCommits = allRefs.map(_.targetObjectId.asRevObject(revWalk)).collect { case c: RevCommit => c }
      revWalk.markStart(startCommits)
      revWalk
    }

    implicit val revWalk = createRevWalk
    implicit val reader = revWalk.getObjectReader

    reporter.reportRefsForScan(allRefs)

    reporter.reportObjectProtection(objectIdCleanerConfig)(repo.getObjectDatabase, revWalk)

    val objectIdCleaner = new BlobSymlinkingObjectIdCleaner(config, objectIdCleanerConfig, repo.getObjectDatabase, revWalk)

    val commits = revWalk.toList

    def clean(commits: Seq[RevCommit]) {
      reporter.reportCleaningStart(commits)

      Timing.measureTask("Cleaning commits", commits.size) {
        Future {
          commits.par.foreach {
            commit => objectIdCleaner(commit.getTree)
          }
        }

        commits.foreach {
          commit =>
            objectIdCleaner(commit)
            progressMonitor update 1
        }
      }
    }

    def updateRefsWithCleanedIds() {
      val refUpdateCommands = for (ref <- repo.nonSymbolicRefs;
                                   (oldId, newId) <- objectIdCleaner.substitution(ref.getObjectId)
      ) yield new ReceiveCommand(oldId, newId, ref.getName)

      if (refUpdateCommands.isEmpty) {
        println("\nAborting: No refs to update - no dirty commits found??\n")
      } else {
        reporter.reportRefUpdateStart(refUpdateCommands)

        Timing.measureTask("...Ref update", refUpdateCommands.size) {
          // Hack a fix for issue #23 : Short-cut the calculation that determines an update is NON-FF
          val quickMergeCalcRevWalk = new RevWalk(revWalk.getObjectReader) {
            override def isMergedInto(base: RevCommit, tip: RevCommit) =
              if (tip == objectIdCleaner(base)) false else super.isMergedInto(base, tip)
          }

          refDatabase.newBatchUpdate.setAllowNonFastForwards(true).addCommand(refUpdateCommands)
            .execute(quickMergeCalcRevWalk, progressMonitor)
        }

        reporter.reportResults(commits, objectIdCleaner)
      }
    }

    clean(commits)
    updateRefsWithCleanedIds()
    objectIdCleaner.stats()
    objectIdCleaner.cleanedObjectMap()
  }
}
