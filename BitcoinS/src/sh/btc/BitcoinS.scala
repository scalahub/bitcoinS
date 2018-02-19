
package sh.btc

import BitcoinUtil._
import sh.btc.DataStructures._
import sh.util.StringUtil._

object BitcoinS {

  var isMainNet = true // set to false for testnet
  
  var defaultUserAgent = "/BitcoinS:0.1/" 

  var ourVersion = 70003 
  var ourServiceBit = 0

  var MagicMainNet = "F9BEB4D9".decodeHex
  var MagicTestNet3 = "0B110907".decodeHex
    
  def getMagicNetBytes = if (isMainNet) MagicMainNet else MagicTestNet3 // we are not handling testnet and namecoin

  /*  Network   Magic value	Sent over wire as
      main      0xD9B4BEF9	F9 BE B4 D9
      testnet   0xDAB5BFFA	FA BF B5 DA 
      testnet3	0x0709110B	0B 11 09 07
      namecoin	0xFEB4BEF9	F9 BE B4 FE   
      
      bitcoinABC  e3:e1:f3:e8
      
   */ 
  
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
