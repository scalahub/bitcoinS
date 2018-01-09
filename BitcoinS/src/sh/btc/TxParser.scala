
package sh.btc

import sh.btc.BitcoinUtil._
import sh.btc.DataStructures._
import sh.util.HashUtil._

class TxParser(bytes:Array[Byte]) extends AbstractParser(bytes) {
  protected def getTxIns = {
    1 to getCompactInt map {_ => // getCompactInt returns num Inputs
      val txHash = getNext32Hash // also converts from little endian to normal
      val vOut = getNext4UInt // signed // can be -1  // should be unsigned as per protocol spec
      // but coinbase inputs have -1 
    
      val scriptSig = getNextBytes(getCompactInt) // getCompactInt returns the size of input script
      val seqNumBytes = getNextBytes(4) // 4 bytes for sequence number      
      val seqNum = getUInt4LittleEndian(seqNumBytes.toArray)
      TxIn(txHash, vOut.toInt).setScriptSig(scriptSig).setSeqNum(seqNum)
      
    }
  }
  protected def getTxOuts = {
    1 to getCompactInt map{_ => // getCompactInt returns num Outputs      
      val value = getUInt8LittleEndian(getNextBytes(8).toArray) // 8 bytes encode Satoshi value in little endian
      val scriptPubKey = getNextBytes(getCompactInt).toArray // getCompactInt returns number of bytes in output script
      TxOut(getAddrFromOutScript(scriptPubKey), value)
    }
  }
  protected def getTxWits(numIns:Int) = {
    1 to numIns map{_ => 
      // 1st getCompactInt returns number of stack items, 2nd getCompactInt returns each stack item's size
      TxWit((1 to getCompactInt).map(_ => getNextBytes(getCompactInt))) 
    }
  }

  def getTx:Tx = {
    val (version, versionBytes) = usingBytes(getNext4SInt) // signed
    val isSegWit = bytes(currCtr) == 0.toByte
    val (ins, outs, flagBytes, wits, inBytes, outBytes, witBytes) = if (isSegWit) { 
      // 1st byte after version is 00 in version bytes, its a segwit tx
      val flagBytes = getNextBytes(2) // skip 2 bytes
      if (flagBytes(1) != 0x01.toByte) throw new Exception(s"Invalid flag byte. Should be 0x01. Found ${flagBytes(1).toHexString}")
      val (ins, inBytes) = usingBytes(getTxIns)
      val (outs, outBytes) = usingBytes(getTxOuts)
      val (wits, witBytes) = usingBytes(getTxWits(ins.size))
      (ins, outs, flagBytes, wits, inBytes, outBytes, witBytes)
    } else { // non-segwit tx
      val (ins, inBytes) = usingBytes(getTxIns)
      val (outs, outBytes) = usingBytes(getTxOuts)
      (ins, outs, Nil, ins.map(_ => TxWit(Nil)), inBytes, outBytes, Nil)
    }
    val (lockTime, lockTimeBytes) = usingBytes(getNext4UInt) // unsigned
    val classicRaw = versionBytes ++ inBytes ++ outBytes ++ lockTimeBytes
    val classicSize = classicRaw.size
    val txId = getHashed(classicRaw)
    val (segWitTxHash, vSize, segWitRaw) = if (isSegWit) {
      val segWitRaw = versionBytes ++ flagBytes ++ inBytes ++ outBytes ++ witBytes ++ lockTimeBytes
      (getHashed(segWitRaw), math.ceil((3 * classicSize + segWitRaw.size)/4d).toInt, segWitRaw)
    } else (txId, classicSize, classicRaw)
    val size = segWitRaw.size
    Tx(version, ins, outs, wits.toArray, lockTime, txId, isSegWit, segWitTxHash, size, vSize)
  }

}
