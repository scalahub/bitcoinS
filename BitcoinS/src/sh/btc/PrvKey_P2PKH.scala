
package sh.btc

import sh.ecc._
import sh.ecc.Util._
import sh.btc._
import sh.btc.BitcoinUtil._
import sh.btc.DataStructures._

class PrvKey_P2PKH (eccPrvKey:ECCPrvKey, mainNet:Boolean) extends PrvKey(eccPrvKey, mainNet) {
  lazy val pubKey = new PubKey_P2PKH(eccPrvKey.eccPubKey, mainNet)
  // ORDINARY 1Address
  def signTx(rawTx:Array[Byte], whichInputs:Seq[(Int, BigInt)]) = signTx_P2PKH(rawTx, whichInputs.unzip._1)
  
  def signTx_P2PKH(rawTx:Array[Byte], whichInputs:Seq[Int]) = { // which input indices to sign
    // https://bitcoin.stackexchange.com/a/37095/2075    
    val tx = new TxParser(rawTx).getTx
    val emptyIns = tx.ins.map(in => new TxIn(in.txHash, in.vOut).setSeqNum(in.seqNum)) // empty = remove all scriptSigs (default is None)
    whichInputs.map{i => 
      val currIn = emptyIns(i)      
      emptyIns(i).setScriptSig(getScriptPubKeyFromAddress(pubKey.address))
      // important !! in P2PKH (nonSegwit), we need to sign old transaction serialized bytes
      val bytesToSign = createNonSegWitTx(tx.version, emptyIns, tx.outs, tx.lockTime) ++ sigHashAllBytes
      val hash = dsha256(bytesToSign)
      val sigBytes = eccPrvKey.signHash(hash) :+ 0x01.toByte // append a 0x01 to indicate SIGHASH_ALL
      emptyIns(i).unsetScriptSig
      tx.ins(i).setScriptSig((sigBytes.size.toByte +: sigBytes) ++ (pubKey.bytes.size.toByte +: pubKey.bytes))
      // set also the corresponding witness to empty
      tx.wits(i) = TxWit(Nil)
    }
    createSegWitTx(tx.version, tx.ins zip tx.wits, tx.outs, tx.lockTime)
  }
}
