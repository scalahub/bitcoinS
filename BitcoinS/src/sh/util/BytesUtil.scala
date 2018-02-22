
package sh.util

object BytesUtil {
  implicit def ByteArrayToBetterByteArray(bytes:Array[Byte]) = new BetterByteArray(bytes)  
  implicit def ByteToBetterByte(byte:Byte) = new BetterByte(byte)  
}

class BetterByteArray(bytes:Seq[Byte]) {
  def encodeHex = Hex.encodeBytes(bytes).toLowerCase
  def encodeBase64 = Base64.encodeBytes(bytes.toArray)
  import BytesUtil._
  def getBits = bytes.map(_.getBits).flatten // lsb last
  def getBitsLsbFirst = bytes.map(_.getBitsLsbFirst).flatten
}

class BetterByte(byte:Byte) {
  def getBits = { // lsb last
    val boolArr = new Array[Boolean](8)
    boolArr(7) = ((byte & 1) != 0);
    boolArr(6) = ((byte & 2) != 0);
    boolArr(5) = ((byte & 4) != 0);
    boolArr(4) = ((byte & 8) != 0);
    boolArr(3) = ((byte & 16) != 0);
    boolArr(2) = ((byte & 32) != 0);
    boolArr(1) = ((byte & 64) != 0);
    boolArr(0) = ((byte & 128) != 0);      
    boolArr
  }
        
  def getBitsLsbFirst = getBits.reverse
}

