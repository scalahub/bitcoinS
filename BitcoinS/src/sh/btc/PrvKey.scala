
package sh.btc

import sh.ecc._
import sh.util.Base58Check
import sh.ecc.Util._
import sh.bch.PrvKey_P2PKH_UAHF
import sh.btc.BitcoinUtil._
import sh.util.BytesUtil._
import sh.util.StringUtil._
import sh.util.BigIntUtil._

/* Explanation: object PrvKey is a companion to class PrvKey
      
        PrvKey (abstract class) constructor has two things:
        1. ECCPrvKey 
        2. Boolean indicating MainNet (true) or TestNet3 (false)
                        
          ECCPrvKey constructor has two things:
          1. BigInt (the actual private key)
          2. Boolean indicating compressed (false) or uncompressed (true)
        -------------------------------------------
        PrvKey has 4 concrete subclasses
          PrvKey_P2PKH
          PrvKey_P2SH_P2PK
          PrvKey_P2SH_P2WPKH
          PrvKey_P2PKH_UAHF
        -------------------------------------------
        PrvKey also encapsulates: 
          PubKey (abstract class), whose constructor has two things
          1. ECCPubKey 
          2. Boolean indicating MainNet (true) or TestNet3 (false)

          ECCPubKey constructor has two things:
          1. Point (the actual EC point)
          2. Boolean indicating compressed (false) or uncompressed (true)
        -------------------------------------------
        PubKey has 4 concrete subclasses, one corresponding to each subclass of PrvKey
          PubKey_P2PKH
          PubKey_P2SH_P2PK
          PubKey_P2SH_P2WPKH
          PubKey_P2PKH_UAHF
        -------------------------------------------
        PubKey also encapsulates: 
          Address (string) depending on the type of network, compression used and subclass pf PubKey
 */
object PrvKey {
  private def getECCPrvKeyAndNet(wif:String) = {
    val bytes = Base58Check.decodePlain(wif).dropRight(4)
    val (mainNetByte, testNetByte) = (0x80.toByte, 0xef.toByte)
    val mainNet = bytes(0) match {
      case `mainNetByte` => true
      case `testNetByte` => false
      case any => ??? // should not happen
    }
    val isCompressed = bytes.last == 0x01 && bytes.size == 34    
    val keyBytes = if (isCompressed) bytes.drop(1).take(32) else bytes.drop(1)
    val eccPrvKey = new ECCPrvKey(keyBytes.encodeHex, isCompressed)
    (eccPrvKey, mainNet)
  }
  // default should be P2PKH
  def getPrvKeyP2PKH(wif:String) = {
    val (eccPrvKey, mainNet) = getECCPrvKeyAndNet(wif)
    new PrvKey_P2PKH(eccPrvKey, mainNet)
  }
  @deprecated("Used only for BCH. Not a part of Bitcoin", "07 Jan 2018")
  def getPrvKeyP2PKH_UAHF(wif:String) = {
    val (eccPrvKey, mainNet) = getECCPrvKeyAndNet(wif)
    new PrvKey_P2PKH_UAHF(eccPrvKey, mainNet)
  }
  def getPrvKeyP2SH_P2PK(wif:String) = {
    val (eccPrvKey, mainNet) = getECCPrvKeyAndNet(wif)
    new PrvKey_P2SH_P2PK(eccPrvKey, mainNet)
  }
  def getPrvKeyP2SH_P2WPKH(wif:String) = {
    val (eccPrvKey, mainNet) = getECCPrvKeyAndNet(wif)
    if (!eccPrvKey.compressed) throw new Exception("P2SH_P2WPKH requires compressed private key")
    new PrvKey_P2SH_P2WPKH(eccPrvKey.bigInt, mainNet)
  }
}
abstract class PrvKey(val eccPrvKey:ECCPrvKey, val mainNet:Boolean) {
  val pubKey:PubKey
  def signTx(rawTx:Array[Byte], inputIndex:Int, amount:BigInt):Array[Byte] = signTx(rawTx, Seq((inputIndex, amount)))
  def signTx(rawTx:Array[Byte], whichInputs:Seq[(Int, BigInt)]):Array[Byte]  

  @deprecated("Should not be exposed", "22 Dec 2017")
  def getWIF:String = getBase58FromBytes( // from BitcoinUtil
    (if (mainNet) 0x80.toByte else 0xef.toByte) +: (
      eccPrvKey.bytes ++ 
      (if (eccPrvKey.compressed) Seq(0x01.toByte) else Nil)
    )
  )
}
