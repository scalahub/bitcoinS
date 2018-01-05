
package sh.btc

import sh.ecc._
import sh.btc._
import sh.ecc.Util._
import sh.btc.BitcoinUtil._
import sh.btc.DataStructures._

class PrvKey_P2SH_P2PK(eccPrvKey:ECCPrvKey, mainNet:Boolean) extends PrvKey(eccPrvKey, mainNet) {
  lazy val pubKey = new PubKey_P2SH_P2PK(eccPrvKey.eccPubKey, mainNet)
  import pubKey._
  import eccPrvKey._
  // P2SH 3Address. This is NOT multisig, but simple Pay to Public Key described in BIP16 (https://github.com/bitcoin/bips/blob/master/bip-0016.mediawiki)
  def signTx(rawTx:Array[Byte], whichInputs:Seq[(Int, BigInt)]) = signTx_P2SH_P2PK(rawTx, whichInputs.unzip._1)
  
  def signTx_P2SH_P2PK(rawTx:Array[Byte], whichInputs:Seq[Int]) = { // which input indices to sign
    val tx = new TxParser(rawTx).getTx // always parse as segwit, since we need to maintain witness data for possible mixed input tx
    whichInputs.map{i => 
      val txHash = tx.getHashSigned_P2SH_P2PK(i, redeemScript) // redeemScript is [pubKeySize] [pubKey] [checkSig]
      val sigBytes = signHash(txHash) :+ 0x01.toByte // ... and we append a 0x01 to indicate SIGHASH_ALL
      /*  Now set the correct scriptSig (with signature) for the newly signed input
          sigBytes will always be <= 65 bytes: https://bitcoin.stackexchange.com/questions/12554/why-the-signature-is-always-65-13232-bytes-long
          which is less than <= 75 (size of anything more than 75 bytes will need to be represented in two or more bytes)_     

          redeemScript is [pubKeySize] [pubKey] [checkSig] 
          (which is at most 1 + 65 + 1 = 67 bytes
          which is less than <= 75 (size of anything more than 75 bytes will need to be represented in two or more bytes)_    */
      tx.ins(i).setScriptSig((sigBytes.size.toByte +: sigBytes) ++ (redeemScript.size.toByte +: redeemScript))      
      tx.wits(i) = TxWit(Nil) // set also the corresponding witness to empty
    }
    createSegWitTx(tx.version, tx.ins zip tx.wits, tx.outs, tx.lockTime) // always parse as segwit, since we need to maintain witness data for possible mixed input tx
  }
}
