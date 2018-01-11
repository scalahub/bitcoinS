
package sh.util

import sh.ecc.Point
import sh.util.StringUtil._

class BetterBigInt (bigInt:BigInt) {
  def *(P:Point) = P * bigInt
  val isEven = (bigInt mod 2) == 0
  val isOdd = !isEven
  val toHex = {
    val h = bigInt.toString(16)
    if (h.size % 2 == 1) "0"+h else h
  }
  def toHex(numBytes:Int):String = {
    val numHex = numBytes * 2
    val hex = toHex
    if (hex.size > numHex) throw new Exception("Error converting $i to hex with $numBytes bytes. Requires ${hex.size/2} bytes")
    val prefix = (1 to (numHex - hex.size) map{_ => "0"} mkString)
    prefix ++ hex
  }
    
  val toBytes = toHex.decodeHex
  def getBigIntBytesBitJ = {
    /* // copied from Bitcoinj source. In case we need to replicate getKeyBytes from BitcoinJ
     * // below is original source
      public static byte[] bigIntegerToBytes(BigInteger b, int numBytes) {
        checkArgument(b.signum() >= 0, "b must be positive or zero");
        checkArgument(numBytes > 0, "numBytes must be positive");
        byte[] src = b.toByteArray();
        byte[] dest = new byte[numBytes];
        boolean isFirstByteOnlyForSign = src[0] == 0;
        int length = isFirstByteOnlyForSign ? src.length - 1 : src.length;
        checkArgument(length <= numBytes, "The given number does not fit in " + numBytes);
        int srcPos = isFirstByteOnlyForSign ? 1 : 0;
        int destPos = numBytes - length;
        System.arraycopy(src, srcPos, dest, destPos, length);
        return dest;
    } */
    if (bigInt < 0) throw new Exception("key must be positive")
    val src = bigInt.toByteArray
    val dest = new Array[Byte](32)
    val isFirstByteOnlyForSign = src(0) == 0;
    val length = if (isFirstByteOnlyForSign) src.length - 1 else src.length;
    if (length > 32) throw new Exception("The given number does not fit in 32 bytes")
    val srcPos = if (isFirstByteOnlyForSign) 1 else 0;
    val destPos = 32 - length;
    System.arraycopy(src, srcPos, dest, destPos, length);
    dest
  }
  
}
object BigIntUtil {
  // below automatically converts BigInt to BetterBigInt when needed
  implicit def bigIntToBetterBigInt(i:BigInt) = new BetterBigInt(i)
  
  // below automatically converts BetterBigInt to BigInt when needed
  implicit def intToBetterBigInt(i:Int) = new BetterBigInt(i)
}
