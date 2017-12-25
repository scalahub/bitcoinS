
package sh.btc

import BitcoinUtil._
import sh.btc.DataStructures._

class TxParser(bytes:Array[Byte]) extends AbstractParser(bytes) {
  def getTx = {
    val versionBytes = getNextBytes(4)
    val isSegWit = bytes(currCtr) == 0.toByte
    val (ins, outs, inBytes, outBytes) = if (isSegWit) { // 1st byte after version is 00 in version bytes, its a segwit tx
      val flagBytes = getNextBytes(2) // skip 2 bytes (flag and marker)
      val (ins, inBytes) = usingBytes(getTxIns)
      val (outs, outBytes) = usingBytes(getTxOuts)
      getTxWits(ins.size) // parse but ignore
      (ins, outs, inBytes, outBytes)
    } else { // non-segwit tx
      val (ins, inBytes) = usingBytes(getTxIns)
      val (outs, outBytes) = usingBytes(getTxOuts)
      (ins, outs, inBytes, outBytes)
    }
    val lockTimeBytes = getNextBytes(4)
    val raw = versionBytes ++ inBytes ++ outBytes ++ lockTimeBytes
    val txId = getHashed(raw)
    TxSimple(ins, outs, txId, raw.size)
  }
  protected def getTxIns = {
    1 to getCompactInt map {i => // getCompactInt returns num Inputs
      val txHash = getNext32Hash
      val vOut = getNext4Int
      val scriptSig = getNextBytes(getCompactInt) // getCompactInt returns the size of input script
      val seqNumBytes = getNextBytes(4) // 4 bytes for sequence number      
      val seqNum = getUInt4LittleEndian(seqNumBytes.toArray)
      In(txHash, vOut.toInt).setScriptSig(scriptSig).setSeqNum(seqNum)
      
    }
  }
  protected def getTxOuts = {
    1 to getCompactInt map{o => // getCompactInt returns num Outputs
      val value = getUInt8LittleEndian(getNextBytes(8).toArray) // 8 bytes encode Satoshi value in little endian
      val scriptPubKey = getNextBytes(getCompactInt).toArray // getCompactInt returns number of bytes in output script
      Out(getAddrFromOutScript(scriptPubKey), value)
    }
  }
  protected def getTxWits(numIns:Int) = {
    1 to numIns map{i => 
      // 1st getCompactInt returns number of stack items, 2nd getCompactInt returns each stack item's size
      Wit((1 to getCompactInt).map(j => getNextBytes(getCompactInt))) 
    }
  }
}
