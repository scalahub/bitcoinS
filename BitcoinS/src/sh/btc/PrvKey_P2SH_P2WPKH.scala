package sh.btc

import sh.ecc._
import sh.btc.BitcoinUtil._
import sh.btc.DataStructures._
import sh.ecc.Util._
import sh.util.BytesUtil._
import sh.util.StringUtil._
import sh.util.BigIntUtil._

class PrvKey_P2SH_P2WPKH (bigInt:BigInt, mainNet:Boolean) extends PrvKey(new ECCPrvKey(bigInt, true), mainNet) {
   
  // if (!eccPrvKey.compressed) throw new Exception("[PrvKey] SegWit requires compressed key")
  
  lazy val pubKey = new PubKey_P2SH_P2WPKH(eccPrvKey.eccPubKey.point, mainNet)
  import pubKey._
  import eccPrvKey._
  // SEGWIT 3Address
  
  def signTx(rawTx:Array[Byte], whichInputs:Seq[(Int, BigInt)]) = { // which input indices to sign, with amount
    val tx = new TxParser(rawTx).getTx
    whichInputs.map{
      case (i, value) => 
        val hash = tx.getHashSigned_P2SH_P2WPKH(i, value, doubleHashedPubKeyBytes)
        val sig = signHash(hash) ++ Array(0x01.toByte) // append a 0x01 to indicate SIGHASH_ALL
        tx.wits(i) = TxWit(Seq(sig, pubKey.bytes))
        // finally set the scriptSig for input script (scriptSig is always a push of redeemScript)
        tx.ins(i).setScriptSig(redeemScript.size.toByte +: redeemScript) // set scriptSig 
    }
    createSegWitTx(tx.version, tx.ins zip tx.wits, tx.outs, tx.lockTime)
  }
}
