
package sh.ecc
import sh.ecc.Util._
import sh.btc.BitcoinS._
import sh.btc.BitcoinUtil._
import sh.util.StringUtil._

object ECCPubKey {
  def apply(secp256k1EncodedHex:String) = {
    // must be hex encoded with sign byte etc
    val signByte = secp256k1EncodedHex.take(2)
    val xHex = secp256k1EncodedHex.drop(2).take(64)
    signByte match {
      case "04" => // uncompressed
        val yHex = secp256k1EncodedHex.drop(66)
        new ECCPubKey(new Point(xHex, yHex), false)
      case "03"|"02" => // compressed
        val x = BigInt(xHex, 16);
        val (even, odd) = Util.findYs(x)
        val y = signByte match {
          case "02" => even 
          case "03" => odd
          case _ => ??? // should not happen      
        }    
        new ECCPubKey(Point(x, y), true)
      case _ => ??? // should not happen      
    }    
  }
}
case class ECCPubKey(point:Point, compressed:Boolean) {

  import point._
  
  val hex:String = {
    if (compressed) {(if (yLowestSetBitIsZero) "03" else "02") + xHex}
    else "04"+xHex+yHex
  }
  val bytes:Array[Byte] = {
    if (compressed) {(if (yLowestSetBitIsZero) 0x03.toByte else 0x02.toByte) +: xBytes}
    else 0x04.toByte +: (xBytes ++ yBytes)
  }
  
  private [sh] def encodeRecoverySig(r:BigInt, s:BigInt, hash:Array[Byte]):Array[Byte] = {
    val recovered = recoverPubKeyPoints(r, s, hash).zipWithIndex.collect{
      case (Some(pk), i) if pk == point => i // valid pub key matching this pub key 
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
    val byteIndex = if (compressed) index + 4 else index 
    encodeRecoverySigForIndex(byteIndex, r, s)
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
