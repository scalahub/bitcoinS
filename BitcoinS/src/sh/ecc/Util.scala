
package sh.ecc

import sh.util.BytesUtil._
import sh.util.StringUtil._
import sh.util.BigIntUtil._

object PointAtInfinityException extends Exception("Point at Infinity")

object Util {
  
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

  val G = ECCPubKey("0279BE667EF9DCBBAC55A06295CE870B07029BFCDB2DCE28D959F2815B16F81798").point
  
  // given x coordinate of a point, returns the ys (even first and odd second)
  def findYs(x:BigInt) = { // returns (even, odd)
    val z = (((x * x) mod p) * x + 7) mod p
    val pMod = ((p + 1) / 4) mod p
    val y1 = z.modPow(pMod, p)
    val y2 = p - y1
    // odd // lowest, i.e., 0th bit is 1)
    if (y1.lowestSetBit == 0) (y2, y1) else (y1, y2)
  } 
  
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
    val hex = bytes.encodeHex
    val bits = BigInt(hex, 16).toString(2)
    (Array.fill(bytes.size * 8 - bits.size)('0') ++ bits).map{
      case '1' => true
      case '0' => false
    }
  }    
  // Needed for RFC6979 (end)
  
  def encodeDERSig(r:BigInt, s:BigInt) = {
    val rBytes = r.toByteArray 
    val rLen = rBytes.size 
    val sBytes = s.toByteArray 
    val sLen = sBytes.size 
    val tLen = 2 + rLen + 2 + sLen
    Array(0x30.toByte, tLen.toByte, 0x02.toByte, rLen.toByte) ++ rBytes ++
    Array(0x02.toByte, sLen.toByte) ++ sBytes 
  }

  def decodeDERSig(signature:String) = {
    decodeDERSigBytes(signature.decodeHex)
  }

  def decodeDERSigBytes(sig:Array[Byte]) = {
    if (sig(0) != 0x30.toByte) throw new Exception(s"Invalid signature: First byte must be ${0x30}. Found ${sig(0)}")    
    val tLen = sig(1).intValue + 2
    if (sig.size != tLen) throw new Exception(s"Expected $tLen bytes in signature. Found ${sig.size} bytes.")
    if (sig(2) != 0x02.toByte) throw new Exception(s"Invalid signature: Third byte must be ${0x02}. Found ${sig(2)}")    
    val rLen = sig(3).intValue
    val r = BigInt(sig.drop(4).take(rLen))
    val right = sig.drop(4+rLen)
    if (right(0) != 0x02.toByte) throw new Exception(s"Invalid signature: ${4+rLen}-th byte must be ${0x02}. Found ${right(0)}")    
    val sLen = right(1).intValue
    if (right.drop(2).size != sLen) throw new Exception(s"Invalid signature: extra bytes after s encoding: ${(right.drop(2+sLen)).encodeHex}")    
    val s = BigInt(right.drop(2)) // IMPORTANT: Use Java Bytes to BigInt conversion, not as hex
    (r, s)
  }

  // check that point satisfies y^2 = x^3 + 7 (mod p)
  def isCurvePoint(x:BigInt, y:BigInt) = ((((y * y) mod p) - 7 - (x modPow (3, p))) mod p) == 0
  
  // key recovery http://www.secg.org/sec1-v2.pdf
  // https://crypto.stackexchange.com/a/18106/81
  def recoverPubKeyPoints(r:BigInt, s:BigInt, hash:Array[Byte]):Seq[Option[Point]] = {
    if (hash.size != 32) throw new Exception(s"Hash must be 32 bytes. Currently ${hash.size} bytes")
    val h = BigInt(hash.encodeHex, 16)
    val rInv = r.modInverse(n)
    (0 to 1).flatMap{j =>
      val x = r + j * n
      if (x >= p) Seq(None, None) else {
        val (y, _) = findYs(x)  
        if (!(isCurvePoint(x, y))) Seq(None, None) else {
          val R = Point(x, y)
          try {
            n * R   // if nR is point at infinity, this will throw an exception
            Seq(None, None) // if here then no exception. nR is not Pt at Infinity
          } catch {
              case PointAtInfinityException => // valid R
                val pt1 = s * R - h * G
                val pt2 = s * (-R) - h * G
                // https://gist.github.com/scalahub/c5801a939f042d092b82f87f2e2ff1db
                // https://gist.github.com/scalahub/cf5af5a9291a07a0331798287a9ad0d9
                Seq(Some(rInv * pt1), Some(rInv * pt2))    // in first, R's y value is even, and in second its odd
          }
        }
      }
    }
  }

  def encodeRecoverySigForIndex(byteIndex:Int, r:BigInt, s:BigInt) = {
    val rBytes = r.toBytes
    val sBytes = s.toBytes
    Array(recoveryEncoding(byteIndex)) ++ 
    Array.fill(32- rBytes.size)(0x00.toByte) ++ rBytes ++ 
    Array.fill(32- sBytes.size)(0x00.toByte) ++ sBytes 
  }
  
  def decodeRecoverySig(bytes:Array[Byte]) = {
    if (bytes.size != 65) throw new Exception(s"Recoverable signature must be 65 bytes. Currently ${bytes.size} bytes")
    val recid = bytes(0)
    val r = BigInt(bytes.drop(1).take(32).encodeHex, 16)
    val s = BigInt(bytes.drop(33).take(32).encodeHex, 16)
    val byteIndex = recoveryEncoding.indexOf(recid)
    if (byteIndex < 0) throw new Exception("Invalid recovery byte "+recid.toHexString)
    (byteIndex, r, s)
  }
  
  def recoverPubKey(recoverySig:Array[Byte], hash:Array[Byte]) = {
  /* https://gist.github.com/scalahub/c5801a939f042d092b82f87f2e2ff1db
`recid`'s encoding might be one of the following:

```
0x1B ->  R_y even | R_x < n | P uncompressed
0x1C ->  R_y odd  | R_x < n | P uncompressed
0x1D ->  R_y even | R_x > n | P uncompressed
0x1E ->  R_y odd  | R_x > n | P uncompressed
0x1F ->  R_y even | R_x < n | P compressed
0x20 ->  R_y odd  | R_x < n | P compressed
0x21 ->  R_y even | R_x > n | P compressed
0x22 ->  R_y odd  | R_x > n | P compressed
```   */
    val (byteIndex, r, s) = decodeRecoverySig(recoverySig)
    val actualIndex = byteIndex % 4 // mod 4 (max 4 distinct keys can be recovered)
    recoverPubKeyPoints(r, s, hash).zipWithIndex.collect{
      case (Some(x), `actualIndex`) =>  // i will be between 0 and 3 // it does not care compressed or uncompressed
        new ECCPubKey(x, byteIndex >= 4)
    }.head
  }
  
  val recoveryEncoding = Array[Byte](0x1B, 0x1C, 0x1D, 0x1E, 0x1F, 0x20, 0x21, 0x22)
  
  val signMagicBytes =  "Bitcoin Signed Message:\n".getBytes

  def getMessageToSignBitcoinD(message:String) = 
    Seq(signMagicBytes.size.toByte) ++ signMagicBytes ++ Seq(message.size.toByte) ++ message.getBytes

}
