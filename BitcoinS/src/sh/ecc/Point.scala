
package sh.ecc

import Util._
import sh.btc.BitcoinUtil._
import sh.btc._
import sh.util.BytesUtil._
import sh.util.StringUtil._
import sh.util.BigIntUtil._
import sh.btc.BitcoinS._
import sh.util.HashUtil._

case class Point(x:BigInt, y:BigInt) {
  if (x >= p || y >= p) throw new Exception("Invalid point. X and Y must be < p")
  if (x <= 0 || y <= 0) throw new Exception("Invalid point. X and Y must be > 0")
  def this(xHex:String, yHex:String) = this(BigInt(xHex, 16) mod p, BigInt(yHex, 16) mod p)
  
  override def toString = x+","+y
  // sanity check
  if (!isCurvePoint(x, y)) throw new Exception("Not a curve point on SECP256k1")
  
  // https://bitcoin.stackexchange.com/a/25448/2075
  def + (that:Point):Point = {
    // below checks if p.x == x but p.y != y (i.e. p is a reflection of this point on x axis)
    val slope = if (that.x == x) { // same point, use point doubling formula instead
      if (that.y != y) throw PointAtInfinityException // we should never encounter this point!      
      (((3 * x * x) mod p) * (2 * y).modInverse(p)) mod p
    } else {
      ((y - that.y) * (x - that.x).modInverse(p)) mod p
    }
    val xSum = (((slope * slope) mod p) - (x + that.x) + p) mod p
    val ySum = (((slope * (x - xSum)) mod p) - y + p) mod p
    
    Point(xSum, ySum)
  }
  
  def * (i:BigInt):Point = {
    if (i == 0) throw PointAtInfinityException
    val str = i.toString(2)
    val binary = str.map{
      case '0' => false
      case '1' => true
    }
    * (binary.reverse)
  }
  
  private def * (bitSeq:Seq[Boolean]):Point = {
    var curr = this    
    bitSeq.zipWithIndex.map{case (bit, i) =>
      if (i > 0) curr = curr.double // first index is 0, where we will skip doubling
      if (bit) Some(curr) else None
    }.collect{
      case Some(p) => p
    }.reduceLeft(_ + _)
  }
  
  // negative of a point
  def unary_- = Point(x, -y mod p) // allows us to write val negP = - P
  
  // point subtraction
  def - (that:Point) = this + (-that) 
  
  // point doubling
  def double:Point = this + this
  
  // verify signature on 32 byte hash (such as hash("Hello")) and signature in r, s form
  def verify(hash:Array[Byte], r:BigInt, s:BigInt) = {
    if (r >= n || s >= n || r < 1 || s < 1) throw new Exception(s"Invalid signature: r and s must be between 1 and $n")    
    if (hash.size != 32) throw new Exception("Hash must be 32 bytes")
    val w = s.modInverse(n)
    val z = BigInt(hash.encodeHex, 16)
    val u1 = (z * w) mod n
    val u2 = (r * w) mod n
    val pt = (u1 * G) + (u2 * this)
    (r mod n) == (pt.x mod n)
  }
  
  def verify(hash:Array[Byte], rs:(BigInt, BigInt)):Boolean = verify(hash, rs._1, rs._2)
 
  def verifyMessageBitcoinD(message:String, sig:String) = {
    // verify signature on msg (human string, such as "Hello") and signature Base64 encoded
    val (_, r, s) = decodeRecoverySig(sig.decodeBase64) // first param is recid. Ignore it
    verify(dsha256(getMessageToSignBitcoinD(message)), r, s)
  }

  val xHex = x.toHex(32) 
  val yHex = y.toHex(32) 
  val xBytes = xHex.decodeHex
  val yBytes = yHex.decodeHex
  val yLowestSetBitIsZero = y.lowestSetBit == 0
  
}
