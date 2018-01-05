
package sh.btc

import sh.btc.BitcoinUtil._
import sh.btc.BitcoinS._
import sh.ecc._
import sh.ecc.Util._
import sh.util.BytesUtil._
import sh.util.StringUtil._
import sh.util.BigIntUtil._

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
      val bytesToSign = createNonSegWitTx(version, emptyIns, outs, lockTime) ++ sigHashAllBytes // important ! in P2PKH (nonSegwit), we need to sign old transaction serialized bytes
      dsha256(bytesToSign)
    }

    // https://bitcoin.stackexchange.com/a/37095/2075    
    def getHashSigned_P2SH_P2PK(whichInput:Int, redeemScript:Seq[Byte]) = {
      val emptyIns = ins.map(in => new TxIn(in.txHash, in.vOut)) // empty = remove all scriptSigs (default is None)
      emptyIns(whichInput).setScriptSig(redeemScript)      
      val bytesToSign = createNonSegWitTx(version, emptyIns, outs, lockTime) ++ sigHashAllBytes // this tx is appended with 01000000 (sigHashAll) ... // important !! in P2SH (nonSegwit), we need to sign old transaction serialized bytes
      dsha256(bytesToSign) // ... and its double hash is signed     
    }

    private val p2sh_p2wpkh_ScriptCode_Prefix = "1976a914".decodeHex
    private val p2sh_p2wpkh_ScriptCode_Suffix = "88ac".decodeHex

    def getHashSigned_P2SH_P2WPKH(whichInput:Int, value:BigInt, hash160PubKey:Seq[Byte]) = { // which input indices to sign, with amount
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
            getVarIntBytes(scriptPubKey.size) ++ scriptPubKey
          }.getOrElse(Seq(0x00.toByte))
        )
      )
      val nLockTime = getFixedIntBytes(lockTime, 4)        
      val nHashType = sigHashAllBytes 
      val bytesToSign = nVer ++ hashPrevOuts ++ hashSeq ++ outPoint ++ scriptCode ++ amt ++ nSeq ++ hashOuts ++ nLockTime ++ nHashType
      dsha256(bytesToSign)
    }

    def isSigned = ins.indices.forall(i => isInputSigned(i))
    
    // To do: implement P2SH validation. Currently only validates P2PKH
    private def isInputSigned(i:Int) = {    
      if (wits(i).data.isEmpty) { // non SegWit Tx
        // i.e., either P2SH_P2PK or P2PKH. We don't deal with P2SH_P2PK. So can skip for now
        val ((r, s), eccPubKey, sigWhatByte) = decodeScriptSigPubKey(ins(i).optScriptSig.getOrElse(throw new Exception(s"Input #$i has not been signed")))
        if (sigWhatByte != 0x01.toByte) throw new Exception(s"Require SIGHASH_ALL appended to signature") 
        eccPubKey.point.verify(getHashSigned_P2PKH(i, new PubKey_P2PKH(eccPubKey, isMainNet).address), r, s)
      } else {
        // validate SegWit. Returning true for now
        true
      }
    }
  
    def isSignedWithAddresses(inAddresses:Seq[Address]) = {    
      if (ins.size != inAddresses.size) throw new Exception("Number of inputs and addresses must be same")
      inAddresses.zipWithIndex.forall{ case (address, i) => isInputSignedWithAddress(i, address)}
    }
    
    // To do: implement P2SH validation. Currently only validates P2PKH
    private def isInputSignedWithAddress(i:Int, inputAddress:Address) = {    
      val ((r, s), eccPubKey, sigWhatByte) = decodeScriptSigPubKey(ins(i).optScriptSig.getOrElse(throw new Exception(s"Input #$i has not been signed")))
      val address = new PubKey_P2PKH(eccPubKey, isMainNet).address
      if (inputAddress != address) false else eccPubKey.point.verify(getHashSigned_P2PKH(i, address), r, s)
    }
  }

  
  class BitcoindBlockSummary(hash:String, prevBlockHash:String, time:Long, version:Long, txHashes:Seq[String]) 
  
  case class BitcoindBlock(
    hash:String, prevBlockHash:String, time:Long, version:Long, txs:Seq[Tx], hexTxs:Seq[String],
    merkleRoot:String, nBits:Seq[Byte], nonce:Long
  ) extends BitcoindBlockSummary(hash, prevBlockHash, time, version, txs.map(_.txid)) {
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
      val txBytesFromHex = hexTxs.flatMap(_.decodeHex)
      val numTxBytes = getVarIntBytes(txs.size)
      val header = versionBytes ++ prevBlockHashBytes++merkleRootBytes ++ timeBytes ++ nBits ++ nonceBytes
      val blk = header ++ numTxBytes ++ txBytesFromHex
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
        currRow(0).reverse.encodeHex.toLowerCase
      }
    }    
  }  
}

