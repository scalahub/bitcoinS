
package sh.btc

import sh.ecc._
import sh.util.Base58Check
import sh.ecc.Util._
import sh.btc.BitcoinUtil._
import sh.util.BytesUtil._
import sh.util.StringUtil._
import sh.util.BigIntUtil._

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
  def getPrvKeyP2SH_P2PK(wif:String) = {
    val (eccPrvKey, mainNet) = getECCPrvKeyAndNet(wif)
    new PrvKey_P2SH_P2PK(eccPrvKey, mainNet)
  }
  def getPrvKeyP2SH_P2WPKH(wif:String) = {
    val (eccPrvKey, mainNet) = getECCPrvKeyAndNet(wif)
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
