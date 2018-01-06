
package sh.btc

import sh.ecc._
import sh.ecc.Util._
import sh.btc._
import sh.btc.BitcoinUtil._
import sh.btc.DataStructures._
import sh.util.StringUtil._

// BCH signing key
@deprecated("Used only for BCH. Not a part of Bitcoin", "07 Jan 2018")
class PrvKey_P2PKH_UAHF (eccPrvKey:ECCPrvKey, mainNet:Boolean) extends PrvKey(eccPrvKey, mainNet) {
  lazy val pubKey = new PubKey_P2PKH(eccPrvKey.eccPubKey, mainNet)
  // ORDINARY 1Address
  def signTx(rawTx:Array[Byte], whichInputsAmts:Seq[(Int, BigInt)]) = signTx_P2PKH(rawTx, whichInputsAmts)
  
  // https://github.com/Bitcoin-UAHF/spec/blob/master/uahf-technical-spec.md
  def signTx_P2PKH(rawTx:Array[Byte], whichInputsAmts:Seq[(Int, BigInt)]) = { // which input indices to sign
    val tx = new TxParser(rawTx).getTx
    whichInputsAmts.map{case (index, value) => 
      val hash = tx.getHashSigned_P2PKH_UASF(index, pubKey.address, value)
      val sigBytes = eccPrvKey.signHash(hash) :+ sigHash_UAHF_Byte // append a 0x40 to indicate SIGHASH_FORKID
      tx.ins(index).setScriptSig((sigBytes.size.toByte +: sigBytes) ++ (pubKey.bytes.size.toByte +: pubKey.bytes))
    }
    createNonSegWitTx(tx.version, tx.ins, tx.outs, tx.lockTime)
  }
}
