package com.gitshed

import java.io.{BufferedWriter, File, FileOutputStream, FileWriter}

import com.madgag.collection.concurrent.ConcurrentMultiMap
import com.madgag.git.ThreadLocalObjectDatabaseResources
import com.madgag.git.bfg.{MemoUtil, Memo, MemoFunc}
import com.madgag.git.bfg.cleaner._
import com.madgag.git.bfg.model.{FileName, Tree}
import org.eclipse.jgit.lib.Constants._
import org.eclipse.jgit.lib.{ObjectChecker, FileMode, ObjectDatabase, ObjectId}
import org.eclipse.jgit.revwalk.RevWalk
import org.eclipse.jgit.util.IO


class BlobSymlinkingObjectIdCleaner(config: Config, objectIdCleanerConfig: ObjectIdCleaner.Config,
                                    objectDB: ObjectDatabase, revWalk: RevWalk)
    extends ObjectIdCleaner(objectIdCleanerConfig, objectDB, revWalk) {

  val blobSymlinker = new BlobSymlinker(config, threadLocalResources, changesByFilename)

  // Override the default behavior and force it to use our BlobSymlinker functionality instead.
  override val cleanTree: MemoFunc[ObjectId, ObjectId] = treeMemo { originalObjectId =>
    blobSymlinker.handleEntry((Nil, new Tree.Entry(null, FileMode.TREE, originalObjectId)))._2.objectId
  }

  override def stats() = super.stats() + ("pathAndEntry" -> blobSymlinker.handleEntry.stats())
}

class BlobSymlinker(config: Config,
                    threadLocalResources: ThreadLocalObjectDatabaseResources,
                    changeRegistry: ConcurrentMultiMap[FileName, (ObjectId, ObjectId)]) {
  // A tree entry and its (reversed) path from the tree root.
  type PathAndEntry = (List[FileName], Tree.Entry)

  val blobLog = new BufferedWriter(new FileWriter(new File("/tmp/bloblog")))

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
    val isBin = isBinary(originalEntry)

    val loader = threadLocalResources.reader().open(originalEntry.objectId)
    val size = loader.getSize.toString
    blobLog.synchronized {
      if (isBin) blobLog.write("B\t") else blobLog.write("T\t")
      blobLog.write(size)
      blobLog.write("\t")
      blobLog.write(originalEntry.objectId.name)
      blobLog.write("\t")
      blobLog.write((path.toArray.reverse map (_.string)).mkString("/"))
      blobLog.write("\n")
    }

    if ((originalEntry.fileMode == FileMode.REGULAR_FILE || originalEntry.fileMode == FileMode.EXECUTABLE_FILE) && isBin) {
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
    val file = new File(config.repoLocation, relpath)
    file.getParentFile.mkdirs()
    val ostr = new FileOutputStream(file)
    threadLocalResources.reader().open(objectId).copyTo(ostr)
    ostr.close()
  }

  def isBinary(entry: Tree.Entry) = !hasTextSuffix(entry.name.string) &&
    (hasBinarySuffix(entry.name.string) || hasBinaryContent(entry.objectId))

  def hasTextSuffix(name: String) = hasSuffix(name, config.textFileSuffixes)

  def hasBinarySuffix(name: String) = hasSuffix(name, config.binaryFileSuffixes)

  def hasSuffix(name: String, suffixSet: Set[String]) = {
    val p = name.lastIndexOf('.')
    ((p >= 0) && suffixSet.contains(name.substring(p))) || suffixSet.contains(name)
  }

  // Detects if a blob is binary (vs. text) the same way git does: looking for a nul byte in some prefix of
  // the content.  This isn't foolproof, but this plus looking at file suffixes gets us close enough.
  // This is somewhat expensive, which is another reason why we first filter out anything we can identify
  // as text or binary by file suffix.
  def hasBinaryContent(objectId: ObjectId) = {
    val buf = new Array[Byte](config.otherBinaryTypeSizeLimitBytes)
    val loader = threadLocalResources.reader().open(objectId)
    val size = Math.min(buf.length, loader.getSize.toInt)
    val in = loader.openStream()
    IO.readFully(in, buf, 0, size)
    in.close()
    var i = 0
    while (i < size && buf(i) != 0.toByte) i += 1
    i < size
  }
}
