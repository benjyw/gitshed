package com.gitshed

import org.eclipse.jgit.lib.{ObjectChecker, FileMode, ObjectDatabase, ObjectId}
import com.madgag.git.bfg.model.Tree
import com.madgag.git.bfg.cleaner.ObjectIdCleaner
import com.madgag.git.bfg.MemoFunc
import org.eclipse.jgit.revwalk.RevWalk
import org.eclipse.jgit.lib.Constants._


class BlobSymlinker(config: ObjectIdCleaner.Config, objectDB: ObjectDatabase, override implicit val revWalk: RevWalk)
    extends ObjectIdCleaner(config, objectDB, revWalk) {
  val objectChecker = new ObjectChecker()
  override val cleanTree: MemoFunc[ObjectId, ObjectId] = treeMemo { originalObjectId =>
    handleEntry(new Tree.Entry(null, FileMode.TREE, originalObjectId)).objectId
  }

  def handleEntry(originalEntry: Tree.Entry): Tree.Entry = originalEntry.fileMode.getObjectType match {
    case OBJ_TREE =>handleTree(originalEntry)
    case OBJ_BLOB => handleBlob(originalEntry)
    case _ => originalEntry
  }

  def handleTree(originalEntry: Tree.Entry): Tree.Entry = {
    val originalChildEntries: Seq[Tree.Entry] = Tree.entriesFor(originalEntry.objectId)(threadLocalResources.reader())
    val newChildEntries: Seq[Tree.Entry] = originalChildEntries map handleEntry
    if (newChildEntries == originalChildEntries) {
      originalEntry
    } else {
      val updatedTree = Tree(newChildEntries)
      val treeFormatter = updatedTree.formatter
      objectChecker.checkTree(treeFormatter.toByteArray)
      val updatedTreeObjectId = treeFormatter.insertTo(threadLocalResources.inserter())
      new Tree.Entry(originalEntry.name, originalEntry.fileMode, updatedTreeObjectId)
    }
  }

  def handleBlob(originalEntry: Tree.Entry): Tree.Entry = {
    if (originalEntry.name.string.startsWith("symlink_me")) {
      new Tree.Entry(originalEntry.name, FileMode.SYMLINK, threadLocalResources.inserter().insert(OBJ_BLOB, "/tmp/symlink".getBytes("UTF-8")))
    } else {
      originalEntry
    }
  }
}
