
package sh.btc

import sh.btc.BitcoinUtil._
import sh.ecc.Util._


object DataStructures {
  
  case class TxIn(txHash:String, vOut:Int) {
    var optScriptSig:Option[Seq[Byte]] = None
    def setScriptSig(scriptSig:Seq[Byte]) = {
      optScriptSig = Some(scriptSig)
      this
    }
    
    var seqNum:Long = BigInt("FFFFFFFF", 16).toLong
    def setSeqNum(long:Long) = {
      this.seqNum = long
      this
    }
    def seqNumBytes = getFixedIntBytes(seqNum, 4) // unsigned
    def unsetScriptSig = optScriptSig = None

    override def toString = txHash+":"+vOut
  }       
  case class TxOut(optAddress:Option[String], value:BigInt) {
    def this (address:String, value:BigInt) = this(Some(address), value)
    override def toString = optAddress.getOrElse("None")+":"+value
    lazy val optScriptPubKey = optAddress.map(getScriptPubKeyFromAddress)
  }        
  
  case class TxWit(data:Seq[Seq[Byte]]) { // witness is a seq of stack element, each a seq of byte
    override def toString = s"witness_{data.size}_bytes"
  }

  case class TxSegWit(
    version:Long, ins:Seq[TxIn], outs:Seq[TxOut], witnesses:Array[TxWit], lockTime:Long, txid:String, 
    isSegWit:Boolean, segWitTxHash:String, size:Int, vSize:Int
  ) {
    def printTx = {
      ins foreach (in => println("In <- "+in))
      outs.zipWithIndex foreach {case (out, i) => println(s"Out -> $txid:$i => "+out)}
      println
    }    
    def serialize = createSegWitTxRawAdvanced(version, ins zip witnesses, outs, lockTime)
  }

  case class TxSimple(version:Long, ins:Seq[TxIn], outs:Seq[TxOut], txid:String, size:Int, lockTime:Long) {
    def printTx = {
      ins foreach (in => println("In <- "+in))
      outs.zipWithIndex foreach {case (out, i) => println(s"Out -> $txid:$i => "+out)}
      println
    }    
    def serialize = createNonSegWitTxRawAdvanced(version, ins, outs, lockTime)
  }
  
  class BitcoindBlockSummary(hash:String, prevBlockHash:String, time:Long, version:Long, txHashes:Seq[String]) 
  
  case class BitcoindBlock(
    hash:String, prevBlockHash:String, time:Long, version:Long, txs:Seq[TxSegWit], hexTxs:Seq[String],
    merkleRoot:String, nBits:Seq[Byte], nonce:Long
  ) extends BitcoindBlockSummary(hash, prevBlockHash, time, version, txs.map(_.txid)) {
    if (nBits.size != 4) throw new Exception("NBits must be exactly 4 bytes")
    def serialize = {
      // header + txs
      // header = version(4)+prevBlkHeaderHash(32)+merkleRoot(32)+4(time)+4(nBits)+4(nonce)
      // println("HEADER HASH = "+getHashed(header))
      val versionBytes = getFixedIntBytes(version, 4)
      val timeBytes = getFixedIntBytes(time, 4) // unsigned
      val nonceBytes = getFixedIntBytes(nonce, 4) // unsigned
      val prevBlockHashBytes = toggleEndianString(prevBlockHash)
      val merkleRootBytes = toggleEndianString(merkleRoot)
      val txBytes = txs.flatMap(_.serialize)
      val txBytesFromHex = hexTxs.flatMap(_.decodeHex)
      val numTxBytes = getVarIntBytes(txs.size)
      val header = versionBytes ++ prevBlockHashBytes++merkleRootBytes ++ timeBytes ++ nBits ++ nonceBytes
      val blk = header ++ numTxBytes ++ txBytesFromHex
      blk
    }
  }
  
  
}

