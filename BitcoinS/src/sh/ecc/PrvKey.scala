
package sh.ecc

import sh.util.Hex
import sh.util.Base58Check

object PrvKey {
  def apply(wif:String) = {
    val bytes = Base58Check.decodePlain(wif).dropRight(4)
    val (mainNetByte, testNetByte) = (0x80.toByte, 0xef.toByte)
    val isMainNetKey = bytes(0) match {
      case `mainNetByte` => true
      case `testNetByte` => false
      case any => ??? // should not happen
    }
    val isCompressed = bytes.last == 0x01 && bytes.size == 34    
    val keyBytes = if (isCompressed) bytes.drop(1).take(32) else bytes.drop(1)
    new PrvKey(Hex.encodeBytes(keyBytes), isCompressed)
  }
}

class PrvKey(key:BigInt, compressed:Boolean) extends PrvKey_P2SH_P2WPKH(key, compressed) {
  def this(hex:String, compressed:Boolean) = this(BigInt(hex, 16), compressed)
}
