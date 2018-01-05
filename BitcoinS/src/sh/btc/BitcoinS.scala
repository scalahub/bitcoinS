
package sh.btc

import BitcoinUtil._
import sh.btc.DataStructures._

object BitcoinS {
  var debug = false
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
  
  def createNonSegWitTxRaw(ins:Seq[TxIn], outs:Seq[TxOut]) = 
    createNonSegWitTx(defaultTxVersion, ins, outs, defaultTxLockTime)
  
  // wrapper over "Advanced", that uses default version, locktime and sets witnesses to empty 
  def createSegWitTxRaw(ins:Seq[TxIn], outs:Seq[TxOut]):Array[Byte] = 
    createSegWitTx(defaultTxVersion, ins.map((_, TxWit(Nil))), outs, defaultTxLockTime)
  
  /* // below is another wrapper, commented out because unused (and should never be needed)
  def createSegWitTxRaw(insWits:Seq[(In, Wit)], outs:Seq[Out]) = 
    createSegWitTxRawAdvanced(defaultVersion, insWits:Seq[(In, Wit)], outs:Seq[Out], defaultLockTime) */
}
