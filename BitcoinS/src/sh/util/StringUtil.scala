
package sh.util

class BetterString(string:String) {
  def decodeHex = Hex.decode(string)
  def decodeBase64 = Base64.decode(string)
}

object StringUtil {
  implicit def StringToBetterString(string:String) = new BetterString(string)
}
