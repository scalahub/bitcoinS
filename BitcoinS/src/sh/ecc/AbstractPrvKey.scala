
package sh.ecc

import Util._
import sh.util.Hex
import sh.btc.BitcoinUtil._


abstract class AbstractPrvKey(protected [sh] val key:BigInt, val compressed:Boolean) {
  if (key >= n) throw new Exception(s"Private key must be < $n")
  if (key <= 0) throw new Exception(s"Private key must be > 0")

  val pubKey = key * G // G is generator of EC

  pubKey.isCompressed = compressed // set compressed info for pub key
  
  protected lazy val keyHex = key.toHex
  protected lazy val keyBytes = key.toBytes 
  
  //val pubKeyHex = pubKey.pubKeyHex 

  @deprecated("Should not be exposed", "22 Dec 2017")
  def getWIF:String = getBase58FromBytes( // from BitcoinUtil
    (if (isMainNet) "80" else "ef")+keyHex + (if (compressed) "01" else "")
  )
  
  val sigHashAllBytes = getFixedIntBytes(0x01, 4)  // 1 implies SIGHASH_ALL // https://en.bitcoin.it/wiki/OP_CHECKSIG
  
  // returns (k, r) in a deterministic way from hash. Requires that hash is already input as SHA256(msg)
  // Follows: https://tools.ietf.org/html/rfc6979
  private def getDeterministicKRFromHash(hash:Array[Byte]) = {
    if (hash.size != 32) throw new Exception("Hash must be 32 bytes")
    val h1 = getBits(hash) // a
    var V = Array.fill(32)(0x01.toByte) // b
    var K = Array.fill(32)(0x00.toByte) // c
    val intToOctets_key = intToOctets(key)
    val bitsToOctets_h1 = bitsToOctets(h1)
    K = HMAC(K)(V ++ Array(0x00.toByte) ++ intToOctets_key ++ bitsToOctets_h1) // d
    V = HMAC(K)(V) // e
    K = HMAC(K)(V ++ Array(0x01.toByte) ++ intToOctets_key ++ bitsToOctets_h1) // f
    V = HMAC(K)(V) // g
    var optKR:Option[(BigInt, BigInt)] = None  // first is k, second is r
    while(optKR.isEmpty) { // h
      var T = Array[Byte]() // h.1 // generates empty byte array
      while(T.size < 32) T = T ++ HMAC(K)(V)  // h.2
      val k = bitsToInt(getBits(T)) // h.3      
      if (k < n && k > 0) { 
        val r = (k * G).x mod n
        if (r != 0) optKR = Some((k, r)) else {
          K = HMAC(K)(V ++ Array(0x00.toByte))
          V = HMAC(K)(V)
        }
      }
    }
    optKR.get
  }
  /*
    https://github.com/bitcoin/bips/blob/master/bip-0066.mediawiki#DER_encoding_reference
     Format: 0x30 [total-length] 0x02 [R-length] [R] 0x02 [S-length] [S] [sighash]
     * total-length: 1-byte length descriptor of everything that follows,
       excluding the sighash byte.
     * R-length: 1-byte length descriptor of the R value that follows.
     * R: arbitrary-length big-endian encoded R value. It must use the shortest
       possible encoding for a positive integers (which means no null bytes at
       the start, except a single one when the next byte has its highest bit set).
     * S-length: 1-byte length descriptor of the S value that follows.
     * S: arbitrary-length big-endian encoded S value. The same rules apply.
     * sighash: 1-byte value indicating what data is hashed (not part of the DER
       signature)
    example: 
    3046022100E755B8C887AF3C97822875908B1BD4566ECAC5BEE9A2BF736C19A4E4BE74F5F8022100A18B52AE9FBE0DE525F6FA58B68D5AC74308886AAC1AA0AB4A7EC55087C85C0C
  */
  private def encodeSigDER(r:BigInt, s:BigInt) = {
    val rBytes = r.toByteArray 
    val rLen = rBytes.size 
    val sBytes = s.toByteArray 
    val sLen = sBytes.size 
    val tLen = 2 + rLen + 2 + sLen
    Array(0x30.toByte, tLen.toByte, 0x02.toByte, rLen.toByte) ++ rBytes ++
    Array(0x02.toByte, sLen.toByte) ++ sBytes 
  }
  def sign(msg:String):String = Hex.encodeBytes(signHash(sha256Bytes2Bytes(msg.getBytes)))

  protected def signHash(hash:Array[Byte]) = {
    if (hash.size != 32) throw new Exception("Hash must be 32 bytes")
    val (k, r) = getDeterministicKRFromHash(hash)
    val h = BigInt(Hex.encodeBytes(hash), 16) mod n
    val s = ((h + key * r) * (k.modInverse(n))) mod n    
    /*  https://github.com/bitcoin/bips/blob/master/bip-0062.mediawiki#Low_S_values_in_signatures
        Low S values in signatures:
        The value S in signatures must be between 
        0x1 and 0x7FFFFFFF FFFFFFFF FFFFFFFF FFFFFFFF 5D576E73 57A4501D DFE92F46 681B20A0 (inclusive). 
        If S is too high, simply replace it by S' = 0xFFFFFFFF FFFFFFFF FFFFFFFF FFFFFFFE BAAEDCE6 AF48A03B BFD25E8C D0364141 - S. */    
    encodeSigDER(r, if (s > sMax) n - s else s)
  }
  
  override def toString = "PrvKey [Hidden] with publicKey: "+pubKey.pubKeyHex
  
}
