package sh.util

object Hex {
  /**
   * Converts a HEX encoded string to an array of bytes.
   * Code is borrowed - need to check correctness.
   * @author http://snippets.dzone.com/posts/show/8027
   */
  def decode(hex: String):Array[Byte] = {
    try {
      (for { i <- 0 to hex.length-1 by 2 if i > 0 || !hex.startsWith( "0x" )}
           yield hex.substring( i, i+2 ))
              .map( Integer.parseInt( _, 16 ).toByte ).toArray
    } catch { case _ : Throwable => Array[Byte](0) }
  }
  def encodeBytes(b:Seq[Byte]):String = b.map("%02X" format _).mkString
}
