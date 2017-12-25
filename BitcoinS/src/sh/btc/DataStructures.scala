
package sh.btc

import sh.btc.BitcoinUtil._
import sh.ecc.Util._


object DataStructures {
  case class In(txHash:String, vOut:Int) {
    var optScriptSig:Option[Seq[Byte]] = None
    def setScriptSig(scriptSig:Seq[Byte]) = {
      optScriptSig = Some(scriptSig)
      this
    }
    
    // below directly stored as serialized (i.e., little endian)
    // and parsed as bytes, not num
    var seqNum:Long = BigInt("FFFFFFFF", 16).toLong
    def setSeqNum(long:Long) = {
      //if (seqNum.size != 4) throw new Exception("Seq number must be 4 bytes")
      this.seqNum = long
      this
    }
    def seqNumBytes = getFixedIntBytes(seqNum, 4)
    def unsetScriptSig = optScriptSig = None

    override def toString = txHash+":"+vOut
  }       
  case class Out(optAddress:Option[String], value:BigInt) {
    override def toString = optAddress.getOrElse("None")+":"+value
    lazy val optScriptPubKey = optAddress.map(getScriptPubKeyFromAddress)
  }        
  case class Wit(data:Seq[Seq[Byte]]) {
    override def toString = s"witness_{data.size}_bytes"
  }

  case class TxSegWit(
    version:Long, ins:Seq[In], outs:Seq[Out], witnesses:Array[Wit], lockTime:Long, txid:String, 
    isSegWit:Boolean, segWitTxHash:String, size:Int, vSize:Int
  ) 

  case class TxSimple(ins:Seq[In], outs:Seq[Out], txid:String, size:Int) {
    def printTx = {
      ins foreach (in => println("In <- "+in))
      outs.zipWithIndex foreach {case (out, i) => println(s"Out -> $txid:$i => "+out)}
      println
    }
    
  }
  
  class BitcoindBlockSummary(hash:String, prevBlockHash:String, time:Long, version:Long, txHashes:Seq[String]) 
  case class BitcoindBlock(
    hash:String, prevBlockHash:String, time:Long, version:Long, txs:Seq[TxSegWit], hexTxs:Seq[String]
  ) extends BitcoindBlockSummary(hash, prevBlockHash, time, version, txs.map(_.txid)) 
  
}

