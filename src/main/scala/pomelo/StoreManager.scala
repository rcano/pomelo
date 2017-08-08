package pomelo

import better.files.{File, StringOps}
import enumeratum.values.{IntEnumEntry, IntEnum}
import java.io.{DataInputStream, DataOutputStream, InputStream, OutputStream, ByteArrayOutputStream}
import java.nio.channels.Channels
import java.nio.file.StandardOpenOption
import java.util.concurrent.ThreadFactory
import java.util.zip.{GZIPInputStream, GZIPOutputStream}

/**
 * StoreManager is responsible for storing files as well as keeping recent updates to it cached, and notifying if the file changed in the
 * file system while the user has it opened.
 * 
 * Intended usage is to instantiate one store manager over a directory that works as its database, and then add files to it.
 */
class StoreManager(databaseDir: File) {

  private final val MaxEntriesPerFile = 20
  
  val logFile = databaseDir / "log"
  private val log: collection.mutable.Map[File, collection.mutable.TreeSet[LogEntry]] = readLog(logFile)
  private val registeredFiles = collection.mutable.Map[File, RegistryHandle]()
  
  private implicit val logEntryOrdering = Ordering.by((le: LogEntry) => -le.timestamp) //newest first
  private def readLog(logFile: File): collection.mutable.Map[File, collection.mutable.TreeSet[LogEntry]] = {
    logFile.createIfNotExists(false, true)
    val res = collection.mutable.Map.empty[File, collection.mutable.TreeSet[LogEntry]]
    
    for (fileChannel <- logFile.fileChannel()) {
      val in = Channels.newInputStream(fileChannel)
      while (fileChannel.position < fileChannel.size) {
        val pos = fileChannel.position
        try {
          val entry = LogEntrySerializer.read(in)
          res.getOrElseUpdate(entry.file, collection.mutable.TreeSet.empty) += entry
        } catch { case ex: Exception => fileChannel.truncate(pos) } //assume any exception as corrupted log from this point onwards, so truncate.
      }
    }
    
    res
  }
  
  /**
   * Returns the files that had dirty writes logged but could not be saved in a previous instance of the program.
   */
  def pendingWrites: Seq[(File, String)] = for {
    (file, entries) <- log.toSeq
    DirtyUpdate(_, ts, content) = entries.firstKey
  } yield  file -> content
  
  /**
   * Registers a file in the StoreManager.
   * 
   * Once registered, you can use the handle to notify of when the text changes, so that the manager can track snapshots of it.
   * You also pass in a callback that the StoreManager will use to notify you whenever the file receives a change in the underlying OS
   * (meaning it was changed outside the StoreManager).
   */
  def register(file: File, changedOnTheBackgroundListener: () => Unit): RegistryHandle = {
    if (registeredFiles.contains(file)) throw new IllegalArgumentException(s"File $file already registered. Cancel the registration before re registering.")
    
    val res = new RegistryHandle {
      var lastUpdate = System.currentTimeMillis()
      def notifyContentChanged(content) = {
        val now = System.currentTimeMillis()
        if (now - lastUpdate > 120 * 1000) {
          lastUpdate = now
          log(DirtyUpdate(file, now, content))(LogEntrySerializer.DirtyUpdateSerializer)
        }
      }
      
      def save(content: String) = {
        file.appendText(content)(File.OpenOptions.append :+ StandardOpenOption.DSYNC, implicitly)
        lastUpdate = file.lastModifiedTime().toEpochMilli
        log(FileSaved(file, lastUpdate))(LogEntrySerializer.FileSavedSerializer)
      }
      
      // Register a file watcher
      // 
      private val watchersThreadFactory: ThreadFactory = runnable => {
        val res = new Thread(null, runnable, s"Watcher: $file", 1024*20) //20kb per watcher
        res.setDaemon(true)
        res
      } 
      val watched = new utils.ThreadBackedFileMonitor(file, watchersThreadFactory, false) {
        override def onModify(file) = {
          if (file.lastModifiedTime().toEpochMilli != lastUpdate) changedOnTheBackgroundListener()
        }
      }
      watched.start()
      
      def cancel() = {
        registeredFiles -= file
        watched.stop()
      }
    }
    registeredFiles(file) = res
    res
  }
  
  
  sealed trait RegistryHandle {
    def notifyContentChanged(content: String): Unit
    def save(content: String): Unit
    def cancel(): Unit
  }
  
  private def log[T <: LogEntry: LogEntrySerializer](entry: T): Unit = {
    val baos = new ByteArrayOutputStream(1000)
    LogEntrySerializer.write(entry, baos)
    logFile.appendByteArray(baos.toByteArray)(File.OpenOptions.append :+ StandardOpenOption.DSYNC)
    val entries = log.getOrElseUpdate(entry.file, collection.mutable.TreeSet.empty)
    entries += entry
    val excess = entries.size - MaxEntriesPerFile
    if (excess > 0) (0 until excess) foreach (_ => entries -= entries.lastKey)
  }
  
  /*******************************
   * Log entries system
   *******************************/
  
  private sealed trait LogEntry {
    def file: File
    def timestamp: Long
  }
  private case class FileSaved(file: File, timestamp: Long) extends LogEntry
  private case class DirtyUpdate(file: File, timestamp: Long, content: String) extends LogEntry
  
  private sealed abstract class LogEntrySerializer[T <: LogEntry](val value: Int) extends IntEnumEntry {
    def write(entry: T, out: OutputStream): Unit
    def read(in: InputStream, timestamp: Long): T
  }
  
  private object LogEntrySerializer extends IntEnum[LogEntrySerializer[_ <: LogEntry]] {
    val values = findValues
    def write[T <: LogEntry](entry: T, out: OutputStream)(implicit serializer: LogEntrySerializer[T]): Unit = {
      val dout = new DataOutputStream(out)
      dout.writeShort(serializer.value)
      serializer.write(entry, out)
    }
    
    def read(in: InputStream): LogEntry = {
      val din = new DataInputStream(in)
      val entryType = din.readShort.toInt
      LogEntrySerializer.withValueOpt(entryType).fold(
        throw new IllegalArgumentException(s"Unknown entry type $entryType"))(serializer => serializer.read(in, din.readLong()))
    }
    
    implicit object FileSavedSerializer extends LogEntrySerializer[FileSaved](10) {
      def write(entry: FileSaved, out) = new DataOutputStream(out).writeUTF(entry.file.pathAsString)
      def read(in, timestamp) = FileSaved(new DataInputStream(in).readUTF().toFile, timestamp)
    }
    
    implicit object DirtyUpdateSerializer extends LogEntrySerializer[DirtyUpdate](20) {
      def write(entry: DirtyUpdate, out) = {
        val zout = new GZIPOutputStream(out)
        val dout = new DataOutputStream(zout)
        dout.writeUTF(entry.file.pathAsString)
        dout.writeUTF(entry.content)
        zout.finish()
      }
      def read(in, timestamp) = {
        val zin = new GZIPInputStream(in)
        val din = new DataInputStream(zin)
        DirtyUpdate(din.readUTF().toFile, timestamp, din.readUTF())
      }
    }
  }
}
