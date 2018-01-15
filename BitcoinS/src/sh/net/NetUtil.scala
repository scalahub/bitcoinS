
package sh.net


import sh.btc.BitcoinUtil._
import sh.btc.AbstractParser
import sh.btc.BitcoinS._
import sh.util.StringUtil._
import sh.net.Payloads.NetAddrPayload
import sh.util.BytesUtil._

object NetUtil {
  def getVarStringBytes(s:String):Seq[Byte] = {
    val bytes = s.toCharArray.toSeq.map(_.toByte)
    getCompactIntBytes(bytes.size) ++ bytes
  }
  def getFixedStringBytes(s:String, size:Int):Seq[Byte] = {
    val bytes = s.toCharArray.toSeq.map(_.toByte)
    if (bytes.size > size) throw new Exception(s"String is too big for $size bytes")
    bytes ++ Seq.fill(size - bytes.size)(0x00.toByte)
  }
  def getTimeSec = getTimeMillis/1000
  def getTimeMillis = System.currentTimeMillis
  
  val MagicMainNet = "F9BEB4D9".decodeHex
  val MagicTestNet3 = "0B110907".decodeHex
    
  def getMagicBytes = if (isMainNet) MagicMainNet else MagicTestNet3 // we are not handling testnet and namecoin

  /*  Network   Magic value	Sent over wire as
      main      0xD9B4BEF9	F9 BE B4 D9
      testnet   0xDAB5BFFA	FA BF B5 DA 
      testnet3	0x0709110B	0B 11 09 07
      namecoin	0xFEB4BEF9	F9 BE B4 FE   */ 
  
  private [net] abstract class UIntX(bigInt:BigInt, sizeBytes:Int) {
    val bytes = getFixedIntBytes(bigInt, sizeBytes)
    val toLong = bigInt.toLong
  }
  /*  UInt32 maps to Long. Range is (0 to 4294967295), well below Long limit
      UInt64 maps to BigInt. Range is (0 to 18446744073709551615)
      UInt16 maps to Int. Range is (0 to 65535), well below Int limit */
  case class UInt16(uint16:Int) extends UIntX(uint16, 2) // Int
  case class UInt32(uint32:Long) extends UIntX(uint32, 4) // Long
  case class UInt64(uint64:BigInt) extends UIntX(uint64, 8) // BigInt
  
  case class IntBool1(boolean:Boolean) extends UIntX(if (boolean) 1 else 0, 1)

  case class Int32(int32:Int) extends UIntX(int32, 4) // Int
  case class Int64(int64:Long) extends UIntX(int64, 8) // Long

  case class VarInt(bigInt:BigInt) {
    val bytes = getCompactIntBytes(bigInt.toLong)
  }
  
  case class Char32(bytes:Seq[Byte]) {
    if (bytes.size != 32) throw new Exception(s"Expcected 32 bytes in Char32. Found ${bytes.size}")
    val rpcHash = bytes.toArray.reverse.encodeHex
  }
  
  implicit def bytesToChar32(bytes:Seq[Byte]) = Char32(bytes)
  private [net] implicit def hash32displayedToChar32(hashRpcHex:String) = bytesToChar32(hashRpcHex.decodeHex.reverse)
  
  // Important: The implicits below might induce bugs elsewhere because they can cause data to be implictly converted without 
  // our knowledge and intention. Use with caution! (and mark as private[net])
  implicit def uInt32ToInt(uInt32:UInt32) = uInt32.toLong.toInt
  implicit def longToUInt32(long:Long) = UInt32(long)
  implicit def stringToUInt32(hex:String) = UInt32(BigInt(hex, 16).toLong)
  implicit def intToUInt16(int:Int) = UInt16(int)
  implicit def intToInt32(int:Int) = Int32(int)
  implicit def bigIntToUInt64(bigInt:BigInt) = UInt64(bigInt)
  implicit def longToInt64(long:Long) = Int64(long)
  implicit def boolToIntBool1(boolean:Boolean) = IntBool1(boolean)
  implicit def intToIntBool1(int:Int) = if (int == 0) false else true
  
  private [net] implicit def intToVarInt(int:Int) = VarInt(BigInt(int))
  private [net] implicit def varIntToInt(varInt:VarInt) = varInt.bigInt.toInt
  
  abstract class AbstractNetParser(bytes:Array[Byte]) extends AbstractParser(bytes) {
    def getString(size:Int) = getNextBytes(size).filter(_ != 0).map(_.toChar).mkString
    def getNext8UInt = BigInt(getNextBytes(8).reverse.toArray.encodeHex, 16)
    def getNext2UInt = BigInt(getNextBytes(2).reverse.toArray.encodeHex, 16).toInt
    def getNetAddrPayload(isVersionMsg:Boolean) = NetAddrPayload(if (isVersionMsg) 0 else getNext4UInt, getNext8UInt, getNextBytes(16).toArray, getNext2UInt)(isVersionMsg)
  }

  
}
