
package sh.util


class BetterByteArray(bytes:Seq[Byte]) {
  def encodeHex = Hex.encodeBytes(bytes)
  def encodeBase64 = Base64.encodeBytes(bytes.toArray)
}
object BytesUtil {
  implicit def ByteArrayToBetterByteArray(bytes:Array[Byte]) = new BetterByteArray(bytes)  
}
