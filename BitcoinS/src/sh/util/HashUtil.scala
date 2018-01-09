package sh.util

import BytesUtil._
import java.security.MessageDigest
import javax.crypto.Mac
import javax.crypto.spec.SecretKeySpec

object HashUtil {

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

  def dsha256(bytes:Seq[Byte]) = sha256Bytes2Bytes(sha256Bytes2Bytes(bytes.toArray))
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
  def hash160(bytes:Seq[Byte]) = ripeMD160(sha256Bytes2Bytes(bytes.toArray))
  
  def getHashed(bytes:Seq[Byte]) = dsha256(bytes.toArray).reverse.encodeHex
  
  
}
