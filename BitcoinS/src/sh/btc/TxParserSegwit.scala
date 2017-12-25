
package sh.btc

import sh.btc.BitcoinUtil._
import sh.btc.DataStructures._

class TxParserSegWit(bytes:Array[Byte]) extends TxParser(bytes) {
  def getSegWitTx = {
    val (version, versionBytes) = usingBytes(getNext4Int)
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
      (ins, outs, Nil, ins.map(in => Wit(Nil)), inBytes, outBytes, Nil)
    }
    val (lockTime, lockTimeBytes) = usingBytes(getNext4Int)
    val classicRaw = versionBytes ++ inBytes ++ outBytes ++ lockTimeBytes
    val size = classicRaw.size
    val txId = getHashed(classicRaw)
    val (segWitTxHash, vSize) = if (isSegWit) {
      val segWitRaw = versionBytes ++ flagBytes ++ inBytes ++ outBytes ++ witBytes ++ lockTimeBytes
      (getHashed(segWitRaw), math.ceil((3 * size + segWitRaw.size)/4d).toInt)
    } else (txId, size)
    TxSegWit(version, ins, outs, wits.toArray, lockTime, txId, isSegWit, segWitTxHash, size, vSize)
  }

}
