package sh.btc

import java.io.File
import java.io.File
import java.nio.file.Files
import scala.collection.mutable.ArrayBuffer
import BitcoinUtil._
import sh.btc.DataStructures._
import sh.util.FileUtil._

trait BlockChainParser {
  val watchAddress:Set[Address] // addresses for which we need to scan blockchain
  println(s"Loaded ${watchAddress.size} address to watch")
  var validUTXO = Map[In, (Address, Amount)]()
  def parseFiles(files:Array[String], startTimeStamp:Long) = {
    files.foreach{file  =>
      System.gc
      println(s"Processing file $file. UXTO size "+validUTXO.size)

      var currBlockByte = -1 // always start with -1, to ensure that its 0 at first iteration (see increment below)
      var currBlock = 0

      val blockSizeBytes = new ArrayBuffer[Byte]
      var endBlockByte = -1 // why is it -1? (because currBlock will become 0 in first iteration, 
      // so there will never be a clash. Any negative number will work

      val blockBytes = new ArrayBuffer[Byte]

      var isInterestingBlock = true // set to true in beginning, until header is parsed. If we find timestamp >= what we want, then its interesting
      val magicBytes = new ArrayBuffer[Byte]

      var validMagicBytes = false

      var optHeader:Option[Array[Byte]] = Some(Array[Byte]())
      // if its not interesting, set to false and stop storing bytes of the block. Just skip it.
      Files.readAllBytes(new File(file).toPath).foreach{ byte =>
        // structure of file is:
        //       ---------------
        // 4 | 4 | 80 | TxData | 4 | 4 | 80 | TxData | 4 | 4 | 80 | TxData | ...
        currBlockByte += 1    // increment current block counter
        if (currBlockByte <= 3) magicBytes += byte
        if (currBlockByte == 3) {
          // magic bytes test              
          if (magicBytes.size != 4) throw new Exception("Magic bytes size should be 4. Found "+magicBytes.size)
          validMagicBytes = (
            magicBytes(0) == 0xF9.toByte && magicBytes(1) == 0xBE.toByte && 
            magicBytes(2) == 0xB4.toByte && magicBytes(3) == 0xD9.toByte 
          )
          magicBytes.clear
        }
        if (currBlockByte >= 4 && currBlockByte <= 7 && validMagicBytes) blockSizeBytes += byte 
        if (currBlockByte == 7 && validMagicBytes) { // first 8 bytes are magicBytes (4) and size of block (4), not part of block encoding
          val numBytesInBlock = getUInt4LittleEndian(blockSizeBytes.toArray).toInt
          endBlockByte = numBytesInBlock + 7
          blockSizeBytes.clear
        }
        if (currBlockByte > 7 && validMagicBytes && isInterestingBlock) blockBytes += byte  // block data
        if (currBlockByte == 87 && validMagicBytes) { // block header is next 80 bytes.. i.e.. 8, 9, ... 87
          // last header byte. Need to parse header as well, to extract timestamp
          val header = blockBytes.toArray // currently collected bytes (80) is header
          optHeader = Some(header)
          if (header.size != 80) throw new Exception("Header size should be 80. Found "+header.size)
          val timeBytes = header.drop(68).take(4)
          val timeStamp = getUInt4LittleEndian(timeBytes)
          if (timeStamp < startTimeStamp) isInterestingBlock = false
        } 
        if (currBlockByte == endBlockByte) {
          // last block byte
          currBlock += 1
          currBlockByte = -1
          endBlockByte = -1
          if (isInterestingBlock && validMagicBytes) {
            val blockParser = new BlockParser(blockBytes.toArray)
            blockParser.txs.foreach{tx =>
              //if (debug) tx.printTx
              val hash = tx.txid
              tx.outs.zipWithIndex.map{case (out, i) =>
                out.optAddress.map{address =>
                  if (watchAddress.contains(address)) validUTXO += new In(hash, i) -> ((address, out.value))
                }
              }
              tx.ins.foreach(in => if (in.vOut >= 0) validUTXO -= in)
            }
          }
          optHeader = None
          blockBytes.clear
          isInterestingBlock = true // set to true for next iteration, to extract timestamp
        } 
      }
    }
  }
  def main(a:Array[String]):Unit = if (a.size >= 3) {
    val dir = a(0) // val dir = "/home/user/.bitcoin/blocks"
    val fromBlkFileNum = a(1).toInt // 0 implies start with blk00000.dat; 1234 implies start with blk01234.dat (going higher)""
    val startTimeStamp = a(2).toLong
    val files = getAllFiles(dir, Array("dat"), false).collect {
      case f if f.contains("blk") => 
        (f, f.drop(s"$dir/blk".size).take(5).toInt)      
    }.sortBy(_._2).drop(fromBlkFileNum).unzip._1 
    println("Num files: "+files.size)
    debug = a.size >= 4 && a(3) == "true"
    parseFiles(files, startTimeStamp)
    println("ValidUTXO.size "+validUTXO.size)
    val id = scala.util.Random.nextInt.abs
    validUTXO.grouped(5000).zipWithIndex.foreach{
      case (validUTXOi, i) => 
        val outFile = "UTXO_"+id+i+".csv"      
        val s = validUTXOi.map{
          case (u, v) => u.txHash+","+u.vOut+", "+v._1+","+v._2
        }.reduceLeft(_+"\n"+_)
        println("Writing to file "+outFile)
        writeToTextFile(outFile, s)
    }
    println("DONE")
  } else println("Usage java -jar <jar> <blockdir> <start_index> <start_timestamp> [debug]")
}
