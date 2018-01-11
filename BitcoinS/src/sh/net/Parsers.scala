
package sh.net

import akka.util.ByteString
import sh.btc.BitcoinS._
import sh.util.StringUtil._
import sh.net.DataStructures._
import sh.util.BytesUtil._
import sh.ecc.Util._
import sh.util.HashUtil._
import sh.btc.BitcoinUtil._
import NetUtil._
import Payloads._

object Parsers {
  
  class MsgParser(val bytes:ByteString) {
    require(bytes.nonEmpty) // sanity check for testing
    val result:Option[(P2PHeader, Option[ByteString])] = new HeaderParser(bytes).header.map(h => (h, new PayloadParser(bytes.drop(headerLen).take(h.payloadLen), h).result))
  }
  
  class HeaderParser(bytes:ByteString) extends AbstractNetParser(bytes.toArray) {
    val header = if (bytes.size >= 24 && getNextBytes(4).toArray.encodeHex == getMagicBytes.encodeHex) { // valid header start
      val command = getString(12)
      val payloadSize = getNext4UInt.toInt
      val checkSum = getNextBytes(4).toArray
      Some(P2PHeader(command, checkSum, payloadSize))
    } else None    
  }

  class PayloadParser(bytes:ByteString, header:P2PHeader) {
    // require (bytes.size <= header.payloadLen) // sanity check for testing
    private val validSize = bytes.size == header.payloadLen
    private val validCheckSum = validSize && header.checkSumHex == dsha256(bytes).take(4).encodeHex
    val result = if (validCheckSum) Some(bytes) else None
    // require(validSize == validCheckSum) // sanity check for testing. If this fails then we are getting invalid packet. We would need to deal with them
  }
  
  class InvPayloadParser(bytes:Array[Byte]) extends AbstractNetParser(bytes) {
    val inv = InvPayload(1 to getCompactInt map(_ => InvVector(getNext4UInt, getNextBytes(32))))
  }

  class AddrPayloadParser(bytes:Array[Byte]) extends AbstractNetParser(bytes) {
    val addr = AddrPayload(1 to getCompactInt map(_ => getNetAddrPayload(false)))
  }

  class VersionPayloadParser(bytes:Array[Byte]) extends AbstractNetParser(bytes) {
    val version = {
      val verInt:Int32 = getNext4UInt.toInt
      val services = getNext8UInt
      val time = getNext8UInt.toLong
      val netAddr1 = getNetAddrPayload(true)
      val netAddr2 = getNetAddrPayload(true)
      val nonce = getNext8UInt
      val userAgent = getString(getCompactInt)
      val lastBlock = getNext4UInt.toInt
      val relay = if (verInt.int32 >= 70001) getNextBytes(1).head else 0
      VersionPayload(verInt, services, time, netAddr1, netAddr2, nonce, userAgent, lastBlock, relay)
    }
  }
  
  class PingPayloadParser(bytes:Array[Byte]) extends AbstractNetParser(bytes) {
    val ping = PingPayload(getNext8UInt)
  }
}
