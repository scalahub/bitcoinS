
package sh.ecc

import Util._
import sh.btc.BitcoinUtil._
import sh.btc._

object Point {
  def apply(secp256k1PointHex:String) = {
    // must be hex encoded with sign byte etc
    val signByte = secp256k1PointHex.take(2)
    val xHex = secp256k1PointHex.drop(2).take(64)
    signByte match {
      case "04" => // uncompressed
        val yHex = secp256k1PointHex.drop(66)
        val p = new Point(xHex, yHex)
        p.isCompressed = false
        p
      case "03"|"02" => // compressed
        val x = BigInt(xHex, 16);
        val (even, odd) = Util.findYs(x)
        val y = signByte match {
          case "02" => even 
          case "03" => odd
          case _ => ??? // should not happen      
        }    
        val p = new Point(x, y)
        p.isCompressed = true
        p
      case _ => ??? // should not happen      
    }    
  }
}
// point and pub key have same structure (both are points on the EC)

case class Point(x:BigInt, y:BigInt) {
  if (x >= p || y >= p) throw new Exception("Invalid point. X and Y must be < p")
  if (x <= 0 || y <= 0) throw new Exception("Invalid point. X and Y must be > 0")
  def this(xHex:String, yHex:String) = this(BigInt(xHex, 16) mod p, BigInt(yHex, 16) mod p)
  var isCompressed = true // default is compressed
  def setCompressed(compressed:Boolean) = {
    isCompressed = compressed
    this
  }
  
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

  // Important! Don't make below to val (keep it def) 
  // since it depends on isCompressed, which is set later after object initialization
  def hex = {
    if (isCompressed) {
      (if (y.lowestSetBit == 0) "03" else "02") + x.toHex(32) 
    } else {
      "04"+x.toHex(32)+y.toHex(32)
    }
  }
  // Important! Don't make below to val (keep it def) 
  // since it depends on isCompressed, which is set later after object initialization
  def bytes = hex.decodeHex
 
  // Important! Don't make below to val (keep it def) 
  // since it depends on isCompressed, which is set later after object initialization
  private [sh] def doubleHashedPubKeyBytes = ripeMD160(sha256Bytes2Bytes(bytes))
    
  /*  Details:
      https://en.bitcoin.it/wiki/Technical_background_of_version_1_Bitcoin_addresses
      https://bitcoin.stackexchange.com/a/3839/2075
      https://en.bitcoin.it/wiki/Base58Check_encoding#Version_bytes  */
  def getAddress = {
    val hash = doubleHashedPubKeyBytes 
    val addrBytes = if (isMainNet) 0x00.toByte +: hash else 111.toByte +: hash
    getBase58FromBytes(addrBytes)
  }
 
  /*  https://github.com/bitcoin/bips/blob/master/bip-0016.mediawiki
      For example, the scriptPubKey and corresponding scriptSig for a one-signature-required transaction is:
      scriptSig: [signature] {[pubkey] OP_CHECKSIG}
      scriptPubKey: OP_HASH160 [20-byte-hash of {[pubkey] OP_CHECKSIG} ] OP_EQUAL
    
      Pubkey script: OP_HASH160 <Hash160(redeemScript)> OP_EQUAL
      Signature script: <sig> [sig] [sig...] <redeemScript>   
   
      public key will be either 65 (uncompressed) or 33 (compressed)
      Thus, size of pubKey is <= 75 and can be represented in one byte (pushdata)  */
  def getRedeemScript_P2SH_P2PK = Seq(bytes.size.toByte)++bytes++Seq(OP_CheckSig)
  
  def getAddress_P2SH_P2PK = { // simple 1 out of 1 P2SH from BIP16
    val redeemScriptHash = hash160(getRedeemScript_P2SH_P2PK.toArray)
    val addrBytes = (if (isMainNet) 0x05.toByte else 0xC4.toByte) +: redeemScriptHash
    getBase58FromBytes(addrBytes) 
  }
  
  def getRedeemScript_P2WPKH = "0014".decodeHex ++ doubleHashedPubKeyBytes   // 00147646c030f7e75b80f0a31cdcab731e6f424f22b2
  /*  To create a P2SH-P2WPKH address:

      Calculate the RIPEMD160 of the SHA256 of a public key (keyhash). 

      The P2SH redeemScript is always 22 bytes. 
      It starts with a OP_0, followed by a canonical push of the keyhash 
      (i.e. 0x0014{20-byte keyhash})

      Same as any other P2SH, the scriptPubKey is 
      OP_HASH160 hash160(redeemScript) OP_EQUAL,  
      and the address is the corresponding P2SH address with prefix 3.  

      The scriptPubKey is the locking script, and is NOT used in computation of the address itself!   */
  
  def getAddress_P2WPKH = {
  /*  Test vector from: https://bitcoin.stackexchange.com/q/60961/2075
      
      Public key - compressed: 
      03fac6879502c4c939cfaadc45999c7ed7366203ad523ab83ad5502c71621a85bb

      SHA256(public key) =
      cfad24b0bc2bba2c8bb2c8d619dca2b74221930793bca50df73856f0bbba10c9

      RIPEMD160(SHA256(public key)) =
      7646c030f7e75b80f0a31cdcab731e6f424f22b2

      redeemScript (OP_0 pubkeyHash160):
      00147646c030f7e75b80f0a31cdcab731e6f424f22b2

      SHA256(redeemScript) =
      a10e523968ba784d24ccd54e613d8f747d6649e42b1df4fdcec6658262620651

      RIPEMD160(SHA256(redeemScript)) =
      188ba16284702258959d8bb63bb9a5d979b57875

      Then do BitcoinUtil.getBase58FromBytes(above value)   */
    if (!isCompressed) throw new Exception("Public key is uncompressed. Segwit needs compressed keys")    
    val redeemScriptHash = hash160(getRedeemScript_P2WPKH) 
    val addrBytes = (if (BitcoinUtil.isMainNet) 0x05.toByte else 196.toByte) +: redeemScriptHash
    BitcoinUtil.getBase58FromBytes(addrBytes)
  }

  def encodeRecoverySig(r:BigInt, s:BigInt, hash:Array[Byte]):Array[Byte] = {
    val recovered = recoverPubKeys(r, s, hash).zipWithIndex.collect{
      case (Some(pk), i) if pk == this => i // valid pub key matching this pub key, note the backticks 
        /*  NOTE: i will be ONE of 0, 1, 2, 3 (i.e., EXACTLY one should match)
            if compressed, we need to add 4 and get corresponding i. See below table
            From: https://gist.github.com/scalahub/c5801a939f042d092b82f87f2e2ff1db  

            recid's encoding might be one of the following:
            0x1B ->  R_y even | R_x < n | P uncompressed
            0x1C ->  R_y odd  | R_x < n | P uncompressed
            0x1D ->  R_y even | R_x > n | P uncompressed
            0x1E ->  R_y odd  | R_x > n | P uncompressed
            0x1F ->  R_y even | R_x < n | P compressed
            0x20 ->  R_y odd  | R_x < n | P compressed
            0x21 ->  R_y even | R_x > n | P compressed
            0x22 ->  R_y odd  | R_x > n | P compressed    */
    }
    if (recovered.size != 1) throw new Exception(s"Recovered keys should be exactly 1. Found ${recovered.size}")
    val index = recovered.head    
    val byteIndex = if (isCompressed) index + 4 else index 
    encodeRecoverySigForIndex(byteIndex, r, s)
  }
  def verifyMessageBitcoinD(message:String, sig:String) = {
    // verify signature on msg (human string, such as "Hello") and signature Base64 encoded
    val (_, r, s) = decodeRecoverySig(sig.decodeBase64) // first param is recid. Ignore it
    verify(dsha256(getMessageToSignBitcoinD(message)), r, s)
  }
  /* // other verify constructs used for testing but not needed in production. Hence commented out
  def verify(msg:String, signature:String):Boolean = {
    val (r, s) = decodeDERSig(signature)
    verify(msg, r, s)
    /*  Example:
	30 46 02 21 00E755B8C887AF3C97822875908B1BD4566ECAC5BEE9A2BF736C19A4E4BE74F5F8
	      02 21 00A18B52AE9FBE0DE525F6FA58B68D5AC74308886AAC1AA0AB4A7EC55087C85C0C */            
  }
  def verifyMessageBitcoinD(message:String, r:BigInt, s:BigInt) = {
    val msg = Seq(magicBytes.size.toByte) ++ magicBytes ++ Seq(message.size.toByte) ++ message.getBytes
    val hash = dsha256(msg)
    verify(hash, r, s)
  }
  
  // verify signature on msg (human string, such as "Hello") and signature in r, s form
  def verify(msg:String, r:BigInt, s:BigInt):Boolean = verify(sha256Bytes2Bytes(msg.getBytes), r, s)
  */

}
