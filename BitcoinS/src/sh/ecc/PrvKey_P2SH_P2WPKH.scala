package sh.ecc

import sh.btc._
import sh.util.Hex
import sh.btc.BitcoinUtil._
import sh.btc.DataStructures._

abstract class PrvKey_P2SH_P2WPKH(key:BigInt, compressed:Boolean) extends PrvKey_P2SH_P2PK(key, compressed) {
  import pubKey._
  // SEGWIT 3Address
  def signTx_P2SH_P2WPKH(rawTx:Array[Byte], whichInputs:Seq[(Int, BigInt)]) = { // which input indices to sign, with amount
    val tx = new TxParserSegWit(rawTx).getSegWitTx
    whichInputs.map{
      case (i, value) => 
        val currIn = tx.ins(i) 
        val nVer = getFixedIntBytes(tx.version, 4)
        val hashPrevOuts = dsha256(
          tx.ins.flatMap(in =>
            Hex.decode(in.txHash).reverse ++ getFixedIntBytes(in.vOut, 4)
          )
        )
        val hashSeq = dsha256(tx.ins.flatMap(in => getFixedIntBytes(in.seqNum, 4)))
        val outPoint = Hex.decode(currIn.txHash).reverse ++ getFixedIntBytes(BigInt(currIn.vOut), 4)                
        val scriptCode = Hex.decode("1976a914") ++ doubleHashedPubKeyBytes ++ Hex.decode("88ac")
        val amt = getFixedIntBytes(value, 8)
        val nSeq = getFixedIntBytes(currIn.seqNum, 4)
        val hashOuts = dsha256(
          tx.outs.flatMap(out =>
            getFixedIntBytes(out.value, 8) ++ out.optScriptPubKey.map(scriptPubKey =>
              getVarIntBytes(scriptPubKey.size) ++ scriptPubKey
            ).getOrElse(Seq(0x00.toByte))
          )
        )
        val nLockTime = getFixedIntBytes(tx.lockTime, 4)        
        val nHashType = sigHashAllBytes 
        val bytesToSign = nVer ++ hashPrevOuts ++ hashSeq ++ outPoint ++ scriptCode ++ amt ++ nSeq ++ hashOuts ++ nLockTime ++ nHashType
        val hash = dsha256(bytesToSign)
        val sign = signHash(hash) ++ Array(0x01.toByte) // append a 0x01 to indicate SIGHASH_ALL
        tx.witnesses(i) = Wit(Seq(sign, pubKey.pubKeyBytes))
        // finally set the scriptSig for input script (scriptSig is always a push of redeemScript)
        val redeemScript = getRedeemScript_P2WPKH
        val scriptSig = redeemScript.size.toByte +: redeemScript
        tx.ins(i).setScriptSig(scriptSig)
    }
    createSegWitTxRaw(tx.ins zip tx.witnesses, tx.outs, tx.lockTime)
  }
}
