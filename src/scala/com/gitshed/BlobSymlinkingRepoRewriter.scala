package com.gitshed

import scala.collection.convert.wrapAll._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

import com.madgag.git._
import com.madgag.git.bfg.Timing
import com.madgag.git.bfg.cleaner._
import com.madgag.git.bfg.cleaner.protection.ProtectedObjectCensus
import org.eclipse.jgit.transport.ReceiveCommand
import org.eclipse.jgit.revwalk.RevSort._
import org.eclipse.jgit.lib.ObjectId
import org.eclipse.jgit.revwalk.{RevWalk, RevCommit}
import org.eclipse.jgit.storage.file.FileRepositoryBuilder
import org.eclipse.jgit.internal.storage.file.FileRepository


// Copied and modified from com.madgag.git.bfg.cleaner.RepoRewriter.
object BlobSymlinkingRepoRewriter {

  def rewrite(config: Config): Map[ObjectId, ObjectId] = {
    val gitdir = resolveGitDirFor(config.repoLocation)
    assert(gitdir.isDefined, "Not a git repo " + config.repoLocation)
    val repo = FileRepositoryBuilder.create(gitdir.get).asInstanceOf[FileRepository]
    assert(!repo.getAllRefs.isEmpty, "Can't find any refs in repo at " + repo.getDirectory.getAbsolutePath)

    val protectedObjectCensus = ProtectedObjectCensus(Set("HEAD"))(repo)
    // Make the following changes to commit messages:
    //   - Rewrite object ids in the message text.
    //   - Add a footer specifying the commit's old id, for future reference.
    val commitNodeCleaners = new CommitMessageObjectIdsUpdater(ObjectIdSubstitutor.OldIdsPublic) :: FormerCommitFooter :: Nil
    val objectIdCleanerConfig = ObjectIdCleaner.Config(
      protectedObjectCensus=protectedObjectCensus,
      commitNodeCleaners=commitNodeCleaners
    )

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
    objectIdCleaner.blobSymlinker.blobLog.close()
    updateRefsWithCleanedIds()
    objectIdCleaner.stats()
    objectIdCleaner.cleanedObjectMap()
  }
}
