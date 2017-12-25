
package sh.btc


import java.io.File
import java.nio.file.Files
import sh.util.FileUtil._
import BitcoinUtil._

object FindTimeStamp {
  // looks for bitcoind blockchain in folder specified as 1st parameter
  // finds the timestamp of the first block in file number given by index (2nd param)
  // For instance if index is 1234, it will look at file blk01234.dat
  def main(a:Array[String]):Unit = if (a.size == 2) {
    val dir = a(0) // val dir = "/home/user/.bitcoin/blocks"
    val fromBlkFileNum = a(1).toInt // 0 implies start with blk00000.dat; 1234 implies start with blk01234.dat (going higher)""
    val files = getAllFiles(dir, Array("dat"), false).collect {
      case f if f.contains("blk") => 
        (f, f.drop(s"$dir/blk".size).take(5).toInt)       // take 5 is based on how bitcoind creates files (5 digits number)
    }.sortBy(_._2).drop(fromBlkFileNum).take(1)
    if (files.isEmpty) throw new Exception("No files found with criteria")
    val (file, num) = files(0)
    val bytes = Files.readAllBytes(new File(file).toPath)
    val size = getUInt4LittleEndian(bytes.drop(4).take(4))
    val header = bytes.drop(8).take(80)
    val timeBytes = header.drop(68).take(4)
    val timeStamp = getUInt4LittleEndian(timeBytes)
    println(s"Timestamp: $timeStamp")
  } else println("Usage java -jar <jar> <blockdir> <start_index>")
  
}
