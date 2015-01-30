package com.gitshed

import java.io.{File, FileOutputStream}

import org.eclipse.jgit.lib.{ObjectChecker, FileMode, ObjectDatabase, ObjectId}
import com.madgag.git.bfg.model.{FileName, Tree}
import com.madgag.git.bfg.cleaner._
import com.madgag.git.bfg.{MemoUtil, Memo, MemoFunc}
import org.eclipse.jgit.revwalk.RevWalk
import org.eclipse.jgit.lib.Constants._
import com.madgag.collection.concurrent.ConcurrentMultiMap
import com.madgag.git.bfg.cleaner.protection.ProtectedObjectCensus
import com.madgag.git.ThreadLocalObjectDatabaseResources


object BlobSymlinkingObjectIdCleaner {
  def makeConfig(protectedObjectCensus: ProtectedObjectCensus) = {
    ObjectIdCleaner.Config(protectedObjectCensus)
  }
}

class BlobSymlinkingObjectIdCleaner(gitshedRoot: String, config: ObjectIdCleaner.Config,
                                    objectDB: ObjectDatabase, revWalk: RevWalk)
    extends ObjectIdCleaner(config, objectDB, revWalk) {

  val blobSymlinker = new BlobSymlinker(gitshedRoot, threadLocalResources, changesByFilename)

  override val cleanTree: MemoFunc[ObjectId, ObjectId] = treeMemo { originalObjectId =>
    blobSymlinker.handleEntry((Nil, new Tree.Entry(null, FileMode.TREE, originalObjectId)))._2.objectId
  }
}

class BlobSymlinker(gitshedRoot: String,
                    threadLocalResources: ThreadLocalObjectDatabaseResources,
                    changeRegistry: ConcurrentMultiMap[FileName, (ObjectId, ObjectId)]) {
  type PathAndEntry = (List[FileName], Tree.Entry)
  val objectChecker = new ObjectChecker()
  val pathAndEntryMemo: Memo[PathAndEntry, PathAndEntry] = MemoUtil.concurrentCleanerMemo[PathAndEntry]()

  val handleEntry: MemoFunc[PathAndEntry, PathAndEntry] = pathAndEntryMemo { pathAndEntry =>
    pathAndEntry._2.fileMode.getObjectType match {
      case OBJ_TREE => handleTree(pathAndEntry)
      case OBJ_BLOB => handleBlob(pathAndEntry)
      case _ => pathAndEntry
    }
  }

  def handleTree(pathAndEntry: PathAndEntry): PathAndEntry = {
    val path = pathAndEntry._1
    val entry = pathAndEntry._2
    val originalChildEntries: Seq[Tree.Entry] = Tree.entriesFor(entry.objectId)(threadLocalResources.reader())
    val newChildEntries: Seq[Tree.Entry] = originalChildEntries map { childEntry =>
      handleEntry((childEntry.name :: path, childEntry))._2
    }
    if (newChildEntries == originalChildEntries) {
      pathAndEntry
    } else {
      val updatedTree = Tree(newChildEntries)
      val treeFormatter = updatedTree.formatter
      objectChecker.checkTree(treeFormatter.toByteArray)
      val updatedTreeObjectId = treeFormatter.insertTo(threadLocalResources.inserter())
      (path, new Tree.Entry(entry.name, entry.fileMode, updatedTreeObjectId))
    }
  }

  def handleBlob(pathAndEntry: PathAndEntry): PathAndEntry = {
    val path = pathAndEntry._1
    val originalEntry = pathAndEntry._2
    if (originalEntry.name.string.startsWith("symlink_me") &&
       (originalEntry.fileMode == FileMode.REGULAR_FILE || originalEntry.fileMode == FileMode.EXECUTABLE_FILE)) {
      val fileName = path.head.string
      val mode = originalEntry.fileMode match {
        case FileMode.REGULAR_FILE => "00444"
        case FileMode.EXECUTABLE_FILE => "00555"
      }
      val gitshedFileName = "%s_%s.%s".format(originalEntry.objectId.name(), mode, fileName)
      val dirParts: Array[String] = path.tail.toArray.reverse map (_.string)
      val gitshedPath = ".gitshed/file/%s/%s".format(dirParts.mkString("/"), gitshedFileName)
      extractBlobContent(originalEntry.objectId, gitshedPath)
      val symlinkTargetRelative = "%s%s".format("../" * dirParts.length, gitshedPath)
      val newEntry = new Tree.Entry(originalEntry.name, FileMode.SYMLINK,
                                    threadLocalResources.inserter().insert(OBJ_BLOB, symlinkTargetRelative.getBytes("UTF-8")))
      changeRegistry.addBinding(originalEntry.name, (originalEntry.objectId, newEntry.objectId))
      (path, newEntry)
    } else {
      pathAndEntry
    }
  }

  def extractBlobContent(objectId: ObjectId, relpath: String) {
    val file = new File(gitshedRoot, relpath)
    file.getParentFile.mkdirs()
    val ostr = new FileOutputStream(file)
    threadLocalResources.reader().open(objectId).copyTo(ostr)
    ostr.close()
  }
}
