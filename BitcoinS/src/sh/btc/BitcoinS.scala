
package sh.btc

import BitcoinUtil._
import sh.btc.DataStructures._
import sh.util.StringUtil._

object BitcoinS {

  var isMainNet = true // set to false for testnet
  
  def isP2SH_Address(address:String) = {
    getKnownScriptPubKey(getScriptPubKeyFromAddress(address)).map{s =>
      s == P2SH
    }.getOrElse(throw new Exception(s"Invalid address $address"))
  }
  def isP2PKH_Address(address:String) = {
    getKnownScriptPubKey(getScriptPubKeyFromAddress(address)).map{s =>
      s == P2PKH
    }.getOrElse(throw new Exception(s"Invalid address $address"))
  }
  def isValidAddress(address:String) = try {
    getKnownScriptPubKey(getScriptPubKeyFromAddress(address)).nonEmpty
  } catch {case a:Any => false}
  
  def isMainNetAddress(address:String) = try {
    val (spk, isMainNetAddress) = getScriptPubKeyAndNetFromAddress(address) 
    getKnownScriptPubKey(spk).nonEmpty && isMainNetAddress
  } catch {case a:Any => false}
  
  // wrapper over "Advanced", that uses default version, locktime and sets witnesses to empty 
  def createTxRaw(ins:Seq[TxIn], outs:Seq[TxOut]) = createNonSegWitTx(defaultTxVersion, ins, outs, defaultTxLockTime)
  
  def signTx(unsignedTx:Array[Byte], inKeysAmts:Seq[(PrvKey, BigInt)]) = 
    inKeysAmts.zipWithIndex.foldLeft(unsignedTx){
      case (prevTx, ((key, amt), i)) => key.signTx(prevTx, i, amt)
    }
  
}
