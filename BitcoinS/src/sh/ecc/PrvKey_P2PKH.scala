
package sh.ecc

import Util._
import sh.btc._
import sh.btc.BitcoinUtil._
import sh.btc.DataStructures._

abstract class PrvKey_P2PKH(key:BigInt, compressed:Boolean) extends AbstractPrvKey(key, compressed) {
  // ORDINARY 1Address
  def signTx_P2PKH(rawTx:Array[Byte], whichInputs:Seq[Int]) = { // which input indices to sign
    // https://bitcoin.stackexchange.com/a/37095/2075    
    val tx = new TxParserSegWit(rawTx).getSegWitTx
    val emptyIns = tx.ins.map(in => new TxIn(in.txHash, in.vOut).setSeqNum(in.seqNum)) // empty = remove all scriptSigs (default is None)
    whichInputs.map{i => 
      val currIn = emptyIns(i)      
      emptyIns(i).setScriptSig(getScriptPubKeyFromAddress(pubKey.getAddress))
      // important !! in P2PKH (nonSegwit), we need to sign old transaction serialized bytes
      val bytesToSign = createNonSegWitTxRawAdvanced(tx.version, emptyIns, tx.outs, tx.lockTime) ++ sigHashAllBytes
      val hash = dsha256(bytesToSign)
      val sigBytes = signHash(hash) :+ 0x01.toByte // append a 0x01 to indicate SIGHASH_ALL
      emptyIns(i).unsetScriptSig
      tx.ins(i).setScriptSig((sigBytes.size.toByte +: sigBytes) ++ (pubKey.bytes.size.toByte +: pubKey.bytes))
    }
    createSegWitTxRawAdvanced(tx.version, tx.ins zip tx.witnesses, tx.outs, tx.lockTime)
  }
}
