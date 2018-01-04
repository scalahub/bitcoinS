package sh.ecc

import sh.btc._
//import sh.util.Hex
import sh.btc.BitcoinUtil._
import sh.btc.DataStructures._
import sh.ecc.Util._
import sh.util.BytesUtil._
import sh.util.StringUtil._
import sh.util.BigIntUtil._

class PrvKey_P2SH_P2WPKH (bigInt:BigInt, mainNet:Boolean) extends PrvKey(new ECCPrvKey(bigInt, true), mainNet) {
   
  if (!eccPrvKey.compressed) throw new Exception("[PrvKey] SegWit requires compressed key")
  
  lazy val pubKey = new PubKey_P2SH_P2WPKH(eccPrvKey.eccPubKey.point, mainNet)
  import pubKey._
  import eccPrvKey._
  // SEGWIT 3Address
  
  def signTx(rawTx:Array[Byte], whichInputs:Seq[(Int, BigInt)]) = { // which input indices to sign, with amount
    println("SIGNING SEGWIT TX")
    val tx = new TxParserSegWit(rawTx).getSegWitTx
    whichInputs.map{
      case (i, value) => 
        val currIn = tx.ins(i) 
        val nVer = getFixedIntBytes(tx.version, 4) // signed As per rules
        val hashPrevOuts = dsha256(
          tx.ins.flatMap(in =>
            in.txHash.decodeHex.reverse ++ getFixedIntBytes(in.vOut, 4) // vOut is signed
          )
        )
        val hashSeq = dsha256(tx.ins.flatMap(in => getFixedIntBytes(in.seqNum, 4))) // unsigned
        val outPoint = currIn.txHash.decodeHex.reverse ++ getFixedIntBytes(BigInt(currIn.vOut), 4)                
        val scriptCode = "1976a914".decodeHex ++ doubleHashedPubKeyBytes ++ "88ac".decodeHex
        val amt = getFixedIntBytes(value, 8) // unsigned
        val nSeq = getFixedIntBytes(currIn.seqNum, 4) // unsigned
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
        tx.wits(i) = TxWit(Seq(sign, pubKey.bytes))
        // finally set the scriptSig for input script (scriptSig is always a push of redeemScript)
        val scriptSig = redeemScript.size.toByte +: redeemScript
        tx.ins(i).setScriptSig(scriptSig) // set scriptSig 
    }
    val raw = createSegWitTxRawAdvanced(tx.version, tx.ins zip tx.wits, tx.outs, tx.lockTime)
    println("raw: "+raw.encodeHex)
    raw
  }
}
