
package sh.ecc

import java.security.MessageDigest
import javax.crypto.Mac
import javax.crypto.spec.SecretKeySpec
import sh.util.Hex

object Util {
  type PubKey = Point
  
  def ripeMD160(raw:Array[Byte]) = {
    // putting below import inside. It will only be invoked if ripeMD160 is called
    // and we can remove the jar if ripeMD160 is not going to be called
    import org.bouncycastle.crypto.digests.RIPEMD160Digest 
    //https://rosettacode.org/wiki/RIPEMD-160#Scala
    val messageDigest = new RIPEMD160Digest
    messageDigest.update(raw, 0, raw.length)
    val out = Array.fill[Byte](messageDigest.getDigestSize)(0)
    messageDigest.doFinal(out, 0)
    out
  }  
  
  val two = BigInt(2)
  // https://en.bitcoin.it/wiki/Secp256k1   
  val p = two.pow(256) - two.pow(32) - two.pow(9) - two.pow(8) - two.pow(7) - two.pow(6) - two.pow(4) - 1
  // note that p = "FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFC2F" (prime)

  // private key must be between 1 and n
  val n =    BigInt("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEBAAEDCE6AF48A03BBFD25E8CD0364141", 16)
  // n is also a prime. (sometimes called q)
  
  // below is needed for high s values test (we need to ensure it is below sMax, which is n/2)
  // https://github.com/bitcoin/bips/blob/master/bip-0062.mediawiki#Low_S_values_in_signatures
  // Note that sMax = (n-1)/2
  val sMax = BigInt("7FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF5D576E7357A4501DDFE92F46681B20A0", 16)

  val G = Point("0279BE667EF9DCBBAC55A06295CE870B07029BFCDB2DCE28D959F2815B16F81798")
  
  // given x coordinate of a point, returns the ys (even first and odd second)
  def findYs(x:BigInt) = { // returns (even, odd)
    val z = (((x * x) mod p) * x + 7) mod p
    val pMod = ((p + 1) / 4) mod p
    val y1 = z.modPow(pMod, p)
    val y2 = p - y1
    // odd // lowest, i.e., 0th bit is 1)
    if (y1.lowestSetBit == 0) (y2, y1) else (y1, y2)
  } 

  // BetterBigInt allows scalar multiplication with point, so we can write
  // i * P, where i is a BigInt and P is a Point.
  // Although BigInt does not have * operation with a point, BetterBigInt does and the implicits
  // below automatically convert BigInt to BetterBigInt and back as and when necessary
  class BetterBigInt (i:BigInt) {
    def *(P:Point) = P * i
    def toHex = {
      val h = i.toString(16)
      if (h.size % 2 == 1) "0"+h else h
    }
    def toBytes = Hex.decode(toHex)
  }

  // below automatically converts BigInt to BetterBigInt when needed
  implicit def bigIntToBetterBigInt(i:BigInt) = new BetterBigInt(i)
  
  // below automatically converts BetterBigInt to BigInt when needed
  implicit def intToBetterBigInt(i:Int) = new BetterBigInt(i)
  
  // below automatically converts string to bytes, assuming string is hex encoded
  // Use with care as it may cause other strings to be converted to bytes
  private [sh] implicit def hexToBytes(hex:String) = Hex.decode(hex)
  
  // below neded for deterministic k value generation
  private def getHMAC(secretKey:Array[Byte]) = {
    val HMAC_SHA256_ALGORITHM = "HmacSHA256";
    // get an hmac_sha1 key from the raw key bytes
    val signingKey = new SecretKeySpec(secretKey, HMAC_SHA256_ALGORITHM);
    // get an hmac_sha1 Mac instance and initialize with the signing key
    val mac = Mac.getInstance(HMAC_SHA256_ALGORITHM);
    mac.init(signingKey);
    mac
  }
  
  def HMAC(secretKey:Array[Byte])(message:Array[Byte]) = getHMAC(secretKey).doFinal(message)
  
  def sha256Bytes2Bytes(b:Array[Byte]):Array[Byte] = MessageDigest.getInstance("SHA-256").digest(b)
  
  // Needed for RFC6979 (start)
  // https://tools.ietf.org/html/rfc6979#section-3.3
  val qLen = 256
  def bitsToInt(bits:Seq[Boolean]) = {
    val newBits = if (bits.size < qLen) 
      (1 to (qLen - bits.size)).map(i => false) ++ bits
    else bits.take(qLen)
    val str = newBits.map{
      case true => '1'
      case false => '0'
    }.mkString
    BigInt(str, 2)
  }
  def intToOctets(int:BigInt) = {
    if (int >= n) throw new Exception(s"Input must be less than $n")
    val bytes = int.toBytes
    if (bytes.size < 32) new Array[Byte](32- bytes.size) ++ bytes else bytes
  }
  def bitsToOctets(bits:Seq[Boolean]) = intToOctets(bitsToInt(bits) mod n)

  def getBits(bytes:Array[Byte]) = {
    val hex = Hex.encodeBytes(bytes)
    val bits = BigInt(hex, 16).toString(2)
    (Array.fill(bytes.size * 8 - bits.size)('0') ++ bits).map{
      case '1' => true
      case '0' => false
    }
  }    
  // Needed for RFC6979 (end)
}
