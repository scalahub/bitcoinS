
package sh.btc

import sh.btc.BitcoinUtil._
import sh.btc.BitcoinS._
import sh.ecc._
import sh.ecc.Util._
import sh.util.BytesUtil._
import sh.util.StringUtil._
import sh.util.BigIntUtil._
import sh.util.HashUtil._

object DataStructures {
  
  case class TxIn(txHash:String, vOut:Int) {
    var optScriptSig:Option[Seq[Byte]] = None
    def setScriptSig(scriptSig:Seq[Byte]) = {
      if (scriptSig.nonEmpty) optScriptSig = Some(scriptSig)
      this
    }
    
    var seqNum:Long = BigInt("FFFFFFFF", 16).toLong
    def setSeqNum(long:Long) = {
      this.seqNum = long
      this
    }
    def seqNumBytes = getFixedIntBytes(seqNum, 4) // unsigned
    def unsetScriptSig = optScriptSig = None
  }       

  case class TxOut(optAddress:Option[String], value:BigInt) {
    def this (address:String, value:BigInt) = this(Some(address), value)    
    lazy val optScriptPubKey = optAddress.map(getScriptPubKeyFromAddress)
  }        
  
  case class TxWit(data:Seq[Seq[Byte]]) // witness is a seq of stack element, each a seq of byte
  
  type Ins = Seq[TxIn]
  type Outs = Seq[TxOut]
  type Wits = Array[TxWit]

  case class Tx(version:Long, ins:Ins, outs:Outs, wits:Wits, lockTime:Long, txid:String, isSegWit:Boolean, segWitTxHash:String, size:Int, vSize:Int) {
    def serialize = createSegWitTx(version, ins zip wits, outs, lockTime)
    
    def getHashSigned_P2PKH(whichInput:Int, inputAddress:String) = {
      val emptyIns = ins.map(in => new TxIn(in.txHash, in.vOut).setSeqNum(in.seqNum)) // empty = remove all scriptSigs (default is None)
      val (scriptPubKey, isMainNetAddr) = getScriptPubKeyAndNetFromAddress(inputAddress)
      if (isMainNetAddr != isMainNet) throw new Exception(s"MainNet mismatch between address and current setting") 
      emptyIns(whichInput).setScriptSig(scriptPubKey)    
      dsha256(createNonSegWitTx(version, emptyIns, outs, lockTime) ++ sigHashAllBytes) // important ! in P2PKH (nonSegwit), we need to sign old transaction serialized bytes
    }
    
    @deprecated("Used only for BCH. Not a part of Bitcoin", "07 Jan 2018")
    def getHashSigned_P2PKH_UAHF(whichInput:Int, inputAddress:String, value:BigInt) = { // which input indices to sign, with amount. Last param is pubKeyHashed via RIPEMD160(SHA256(_))
      val currIn = ins(whichInput) 
      val nVer = getFixedIntBytes(version, 4) // signed As per rules
      val hashPrevOuts = dsha256(ins.flatMap(in => in.txHash.decodeHex.reverse ++ getFixedIntBytes(in.vOut, 4)))  // vOut is signed
      val hashSeq = dsha256(ins.flatMap(in => getFixedIntBytes(in.seqNum, 4))) // unsigned
      val outPoint = currIn.txHash.decodeHex.reverse ++ getFixedIntBytes(BigInt(currIn.vOut), 4)                

      val (scriptPubKey, isMainNetAddr) = getScriptPubKeyAndNetFromAddress(inputAddress)
      if (isMainNetAddr != isMainNet) throw new Exception(s"MainNet mismatch between address and current setting") 

      val scriptCode = scriptPubKey.size.toByte +: scriptPubKey
      val amt = getFixedIntBytes(value, 8) // unsigned
      val nSeq = getFixedIntBytes(currIn.seqNum, 4) // unsigned
      val hashOuts = dsha256(
        outs.flatMap(out =>
          getFixedIntBytes(out.value, 8) ++ out.optScriptPubKey.map{scriptPubKey =>
            getCompactIntBytes(scriptPubKey.size) ++ scriptPubKey
          }.getOrElse(Seq(0x00.toByte))
        )
      )
      val nLockTime = getFixedIntBytes(lockTime, 4)        
      val nHashType = sigHashAll_UAHF_Bytes 
      dsha256(nVer ++ hashPrevOuts ++ hashSeq ++ outPoint ++ scriptCode ++ amt ++ nSeq ++ hashOuts ++ nLockTime ++ nHashType)
    }

    // https://bitcoin.stackexchange.com/a/37095/2075    
    def getHashSigned_P2SH_P2PK(whichInput:Int, redeemScript:Seq[Byte]) = {
      val emptyIns = ins.map(in => new TxIn(in.txHash, in.vOut)) // empty = remove all scriptSigs (default is None)
      emptyIns(whichInput).setScriptSig(redeemScript)      
      dsha256(createNonSegWitTx(version, emptyIns, outs, lockTime) ++ sigHashAllBytes ) // this tx is appended with 01000000 (sigHashAll) ... // important !! in P2SH (nonSegwit), we need to sign old transaction serialized bytes ... and its double hash is signed     
    }

    private val p2sh_p2wpkh_ScriptCode_Prefix = "1976a914".decodeHex
    private val p2sh_p2wpkh_ScriptCode_Suffix = "88ac".decodeHex

    def getHashSigned_P2SH_P2WPKH(whichInput:Int, value:BigInt, hash160PubKey:Seq[Byte]) = { // which input indices to sign, with amount. Last param is pubKeyHashed via RIPEMD160(SHA256(_))
      val currIn = ins(whichInput) 
      val nVer = getFixedIntBytes(version, 4) // signed As per rules
      val hashPrevOuts = dsha256(ins.flatMap(in => in.txHash.decodeHex.reverse ++ getFixedIntBytes(in.vOut, 4)))  // vOut is signed
      val hashSeq = dsha256(ins.flatMap(in => getFixedIntBytes(in.seqNum, 4))) // unsigned
      val outPoint = currIn.txHash.decodeHex.reverse ++ getFixedIntBytes(BigInt(currIn.vOut), 4)                
      val scriptCode = p2sh_p2wpkh_ScriptCode_Prefix ++ hash160PubKey ++ p2sh_p2wpkh_ScriptCode_Suffix
      val amt = getFixedIntBytes(value, 8) // unsigned
      val nSeq = getFixedIntBytes(currIn.seqNum, 4) // unsigned
      val hashOuts = dsha256(
        outs.flatMap(out =>
          getFixedIntBytes(out.value, 8) ++ out.optScriptPubKey.map{scriptPubKey =>
            getCompactIntBytes(scriptPubKey.size) ++ scriptPubKey
          }.getOrElse(Seq(0x00.toByte))
        )
      )
      val nLockTime = getFixedIntBytes(lockTime, 4)        
      val nHashType = sigHashAllBytes 
      dsha256(nVer ++ hashPrevOuts ++ hashSeq ++ outPoint ++ scriptCode ++ amt ++ nSeq ++ hashOuts ++ nLockTime ++ nHashType)
    }

    def isSigned(values:Seq[BigInt], optAddressSeq:Option[Seq[Address]] = None) = {
      if (ins.size != values.size) throw new Exception("Number of inputs and values must be same. Set to any random value for non-SegWit input")
      val optAddresses = if (optAddressSeq.isDefined) optAddressSeq.get.map(Some(_)) else Seq.fill(ins.size)(None)
      if (ins.size != optAddresses.size) throw new Exception("Number of inputs and addresses must be same")
      values.zip(optAddresses).zipWithIndex.forall{case ((value, optAddress), i) => isInputSigned(i, value, optAddress)}
    }
    
    private def isInputSigned(index:Int, value:BigInt, optAddress:Option[Address]) = {    
      if (wits(index).data.isEmpty) { // non SegWit Tx
        if (ins(index).optScriptSig.isEmpty) false else {
          /* p2pkh:     scriptSig is [sigSize][Sig][pubKeySize][pubKey]
             p2sh_p2pk: scriptSig is [sigSize][Sig][redeemScriptSize][[pubKeySize][pubKey][checkSig]]
            (i.e., it of the form    [sigSize][Sig][redeemScriptSize][redeemScript], where redeemScript is [pubKeySize][pubKey][checkSig])    */
          val scriptSig = ins(index).optScriptSig.get
          val sigSize = scriptSig(0).toInt
          val sigBytes = scriptSig.drop(1).take(sigSize)
          val (sigDER, sigHashWhat) = (sigBytes.init.toArray, sigBytes.last)
          if (sigHashWhat != 0x01.toByte) throw new Exception(s"Require SIGHASH_ALL appended to signature")           
          
          val remainingSize = scriptSig(sigSize+1).toInt
          val remaining = scriptSig.drop(sigSize+2).take(remainingSize).toArray
          
          val (point, hash, address) = if (remainingSize == 33 || remainingSize == 65) { // public key (33 is compressed, 65 is uncompressed) 
            // its a P2PKH input
            val eccPubKey = ECCPubKey(remaining.encodeHex)
            val pubKey = new PubKey_P2PKH(eccPubKey, isMainNet)
            (eccPubKey.point, getHashSigned_P2PKH(index, pubKey.address), pubKey.address)
          } else { // remaining = redeemScript = [pubKeySize][pubKey][checkSig]
            // its a P2SH_P2PL input (or some other P2SH input, but we only support P2SH_P2PK and P2SH_P2WPKH -- see below)
            if (remaining.last != OP_CheckSig) throw new Exception(s"Invalid scriptSig for input #$index")
            val eccPubKey = ECCPubKey(remaining.drop(1).take(remaining.head).encodeHex)
            (eccPubKey.point, getHashSigned_P2SH_P2PK(index, remaining), new PubKey_P2SH_P2PK(eccPubKey, isMainNet).address)
          }
          if (optAddress.getOrElse(address) != address) false else point.verify(hash, decodeDERSigBytes(sigDER))
        } 
      } else { // P2SH_P2WPKH, SegWit input
        val (sig, sigWhatByte) = (wits(index).data(0).init, wits(index).data(0).last)
        if (sigWhatByte != 0x01.toByte) throw new Exception(s"Require SIGHASH_ALL appended to signature") 
        val eccPubKey = ECCPubKey(wits(index).data(1).toArray.encodeHex)
        if (!eccPubKey.compressed) throw new Exception(s"SegWit public keys must be compressed for index $index")
        val pubKey = new PubKey_P2SH_P2WPKH(eccPubKey.point, isMainNet)
        if (optAddress.getOrElse(pubKey.address) != pubKey.address) false else eccPubKey.point.verify(getHashSigned_P2SH_P2WPKH(index, value, pubKey.doubleHashedPubKeyBytes), decodeDERSigBytes(sig.toArray))
      }
    }
  }

  class BlkSummary(hash:String, prevBlockHash:String, time:Long, version:Long, txHashes:Seq[String]) 
  
  case class Blk(
    hash:String, prevBlockHash:String, time:Long, version:Long, txs:Seq[Tx],
    merkleRoot:String, nBits:Seq[Byte], nonce:Long
  ) extends BlkSummary(hash, prevBlockHash, time, version, txs.map(_.txid)) {
    if (nBits.size != 4) throw new Exception("NBits must be exactly 4 bytes")
    def serialize = {
      // block is serialized as header + numTxs + txs
      // header = version(4)+prevBlkHeaderHash(32)+merkleRoot(32)+4(time)+4(nBits)+4(nonce)
      val versionBytes = getFixedIntBytes(version, 4)
      val timeBytes = getFixedIntBytes(time, 4) // unsigned
      val nonceBytes = getFixedIntBytes(nonce, 4) // unsigned
      val prevBlockHashBytes = toggleEndianString(prevBlockHash)
      val merkleRootBytes = toggleEndianString(merkleRoot)
      val txBytes = txs.flatMap(_.serialize)
      val numTxBytes = getCompactIntBytes(txs.size)
      val header = versionBytes ++ prevBlockHashBytes++merkleRootBytes ++ timeBytes ++ nBits ++ nonceBytes
      val blk = header ++ numTxBytes ++ txBytes
      blk
    }
    def computeMerkleRoot = {
      /* Computing the Merkle root: The transactions are first arranged in some
      order that satisfies the consensus rules given below. Their transaction hashes
      (TXIDs) are considered as the last row (leaves) of the tree that will be constructed.
      Starting with the last row, each row is iteratively processed to get the
      previous (parent) row until the currently processing row has only one node, the
      Merkle root. If the currently processing row has two or more nodes, we first
      ensure that there are even number (say n) of them, by adding a null element
      if necessary. Then we pair the nodes to form n/2 pairs. Each pair (L, R) is
      concatenated and its hash SHA256(SHA256(L||R)) forms the parent for the next
      iteration. This process is repeated until the root is reached. */    
      // https://bitcoin.org/en/developer-reference#merkle-trees
      if (txs.size == 1) txs(0).txid // only coinbase
      else {
        var currRow = txs.map(_.txid.decodeHex.reverse)
        while (currRow.size > 1) {
          val newCurrRow = if (currRow.size.isEven) currRow else currRow :+ currRow.last
          currRow = newCurrRow.grouped(2).toSeq.map(a => dsha256(a(0) ++ a(1)))
        }
        currRow(0).reverse.encodeHex
      }
    }    
  }  
}

