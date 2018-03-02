
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
  
  // https://bitcoin.stackexchange.com/a/37095/2075    
  def signTx_P2PKH(rawTx:Array[Byte], whichInputs:Seq[Int]) = { // which input indices to sign
    val tx = new TxParser(rawTx).getTx
    whichInputs.map{i => 
      val hash = tx.getHashSigned_P2PKH(i, pubKey.address)
      val sigBytes = eccPrvKey.signHash(hash) :+ 0x01.toByte // append a 0x01 to indicate SIGHASH_ALL
      tx.ins(i).setScriptSig((sigBytes.size.toByte +: sigBytes) ++ (pubKey.bytes.size.toByte +: pubKey.bytes))
      tx.wits(i) = TxWit(Nil)       // set also the corresponding witness to empty
    }
    createSegWitTx(tx.version, tx.ins zip tx.wits, tx.outs, tx.lockTime)
  }
}
