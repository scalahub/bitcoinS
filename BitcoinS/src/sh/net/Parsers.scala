
package sh.net

import akka.util.ByteString
import sh.btc.BitcoinS._
import sh.util.StringUtil._
import sh.net.DataStructures._
import sh.net.Parsers.RejectPayloadParser
import sh.util.BytesUtil._
import sh.btc.BlockHeaderParser
import sh.ecc.Util._
import sh.util.HashUtil._
import sh.btc.BitcoinUtil._
import NetUtil._
import Payloads._

object Parsers {
  
  class MsgParser(magicBytes:Array[Byte], val bytes:ByteString) {
    val result:Option[(P2PHeader, Option[ByteString])] = new HeaderParser(magicBytes, bytes).header.map(h => (h, new PayloadParser(bytes.drop(headerLen).take(h.payloadLen), h).result))
  }
  
  class HeaderParser(magicBytes:Array[Byte], bytes:ByteString) extends AbstractNetParser(bytes.toArray) {
    val header = if (bytes.size >= 24 && (getNextBytes(4) zip magicBytes).forall{case (a,b)=> a == b}) { // valid header start
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
  
  /*
   * REJECT CODES: https://gist.github.com/gavinandresen/7079034
   *
Range	Category
0x01-0x0f	Protocol syntax errors
0x10-0x1f	Protocol semantic errors
0x40-0x4f	Server policy rule
rejection codes common to all message types

Code	Description
0x01	Message could not be decoded

   */
  case class RejectMessage(msg:String, code:Byte, reason:String, details:Seq[Byte]) {
    val category = code match {
      case a if a >= 0x01 && a <= 0x0f => "Protocol syntax errors"
      case a if a >= 0x10 && a <= 0x1f => "Protocol semantic errors"
      case a if a >= 0x40 && a <= 0x4f => "Server policy rule"
    }
    val cause = code match {
      case 0x01 => "Message could not be decoded"

      case 0x10 => "Transaction is invalid for some reason (invalid signature, output value greater than input, etc.)"        
      case 0x40	=> "Not mined/relayed because it is 'non-standard' (type or version unknown by the server)"
      case 0x41	=> "One or more output amounts are below the 'dust' threshold"
      case 0x42	=> "Transaction does not have enough fee/priority to be relayed or mined"        

      case 0x11 => "Client is an obsolete, unsupported version"
      case 0x12 => "Duplicate version message received"
      case _ => "Unknown"
    }
    override def toString = s"Reject:$msg:$code:$reason:${details.toArray.reverse.encodeHex}"
  }
/*
reject version codes
Codes generated during the intial connection process in response to a "version" message:

Code	Description
0x11	Client is an obsolete, unsupported version
0x12	Duplicate version message received

reject tx payload, codes
Transaction rejection messages extend the basic message with the transaction id:

Field Size	Description	Data type	Comments
32              txid            char[32]	transaction that is rejected
The following codes are used:

Code	description
0x10	Transaction is invalid for some reason (invalid signature, output value greater than input, etc.)
0x40	Not mined/relayed because it is "non-standard" (type or version unknown by the server)
0x41	One or more output amounts are below the 'dust' threshold
0x42	Transaction does not have enough fee/priority to be relayed or mined

payload, reject block
Block rejection messages extend the basic message with the block header hash:

Field Size	Description	Data type	Comments
32              hash            char[32]	block (hash of block header) that is rejected

Rejection codes:
code	description
0x10	Block is invalid for some reason (invalid proof-of-work, invalid signature, etc)
0x11	Block's version is no longer supported
0x43	Inconsistent with a compiled-in checkpoint

Note: blocks that are not part of the server's idea of the current best chain, but are otherwise valid, should not trigger "reject" messages.

Implementation notes
Sending "reject" messages to old nodes does no harm-- unknown commands are ignored for extensibility (in the reference implementation, at least-- other implementations should do the same). So there is no need to bump the protocol version.

Implementors should consider what happens if an attacker either sends them "reject" messages for valid transactions/blocks or sends them random "reject" messages, and should beware of possible denial-of-service attacks (e.g. blindly notifying the user of every "reject" message received would definitely be the wrong thing to do, and even blindly writing every "reject" message to a debug.log could open up a fill-up-disk DoS attack).

 */  
  class RejectPayloadParser(bytes:Array[Byte]) extends AbstractNetParser(bytes) {
    val msg = getString(getCompactInt) // response-to-msg	var_str	Message that triggered the reject
    val code = getNextBytes(1).head // reject-code	uint8_t
    val debugMsg = getString(getCompactInt) // reason	var_str	Human-readable message for debugging
    lazy val remaining = if (numBytesRemaining > 0) getNextBytes(numBytesRemaining) else Nil
    val rej = RejectMessage(msg, code, debugMsg, remaining)
    if (Peer.debug) println("Rej: "+bytes.encodeHex)
    
    
    /*
The reject message is sent when messages are rejected.
Payload:
Field Size	Description	Data type	Comments
1+	message	var_str	type of message rejected
1	ccode	char	code relating to rejected message
1+	reason	var_str	text version of reason for rejection
0+	data	char	Optional extra data provided by some errors. Currently, all errors which provide this field fill it with the TXID or block header hash of the object being rejected, so the field is 32 bytes.
     */
    
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
  
  class MerkleBlockPayloadParser(bytes:Array[Byte]) extends AbstractNetParser(bytes) {
    
    val merkleBlock = {
      val header = new BlockHeaderParser(getNextBytes(80).toArray).header
      val txCount = getNext4UInt.toInt
      val hashes = 1 to getCompactInt map (_ => getNextBytes(32).toArray)
      val flags = getNextBytes(getCompactInt).toArray
      MerkleBlock(header, txCount, hashes, flags)
    }
  }
  
  class PingPayloadParser(bytes:Array[Byte]) extends AbstractNetParser(bytes) {
    val ping = PingPayload(getNext8UInt)
  }
}
