
package sh.net

import java.net.InetSocketAddress
import java.net.Socket
import sh.btc.BitcoinS._
import sh.util.StringUtil._
import sh.net.DataStructures._
import sh.util.BytesUtil._
import sh.ecc.Util._
import sh.util.HashUtil._
import sh.btc.BitcoinUtil._
import NetUtil._

object Payloads {
  
  case class PingPayload(nonce:UInt64) extends Payload(nonce.bytes)
  case class PongPayload(ping:PingPayload) extends Payload(ping.nonce.bytes)
  
  case class InvPayload(invVectors:Seq[InvVector]) extends {   
    val numItems:VarInt = invVectors.size
  } with Payload(numItems.bytes ++ invVectors.flatMap(_.bytes)) {
    def this(invVector:InvVector) = this(Seq(invVector))
  }
  
  case class AddrPayload(netAddrs:Seq[NetAddrPayload]) extends {
    val numItems:VarInt = netAddrs.size
  } with Payload(numItems.bytes ++ netAddrs.flatMap(_.bytes)) {
    def this(netAddr:NetAddrPayload) = this(Seq(netAddr))
    def this(address:Array[Byte], port:Int) = this(new NetAddrPayload(address, port))
    def this(sAddress:InetSocketAddress) = this(new NetAddrPayload(sAddress.getAddress.getAddress, sAddress.getPort))
  }
  
  case class VersionPayload(
    version:Int32, services:UInt64, timeStamp:Int64, addr_recv:NetAddrPayload, addr_from:NetAddrPayload,
    nonce:UInt64, userAgent:String, startHeight:Int32, relay:Boolean
  ) extends Payload(
    version.bytes ++ services.bytes ++ timeStamp.bytes ++ addr_recv.bytes ++ addr_from.bytes ++ 
    nonce.bytes ++ getVarStringBytes(userAgent) ++ startHeight.bytes ++ relay.bytes
  ) {
    def this(socket:Socket) = this(
      ourVersion, BigInt(0), getTime, 
      new NetAddrPayload(socket.getLocalAddress.getAddress, socket.getLocalPort, true),
      new NetAddrPayload(socket.getInetAddress.getAddress, socket.getPort, true),
      BigInt(nonce.getAndIncrement), defaultUserAgent, 1, true
    )
    def this(local:InetSocketAddress, remote:InetSocketAddress) = this(
      ourVersion, BigInt(0), getTime, 
      new NetAddrPayload(local.getAddress.getAddress, local.getPort, true),
      new NetAddrPayload(remote.getAddress.getAddress, remote.getPort, true),
      BigInt(nonce.getAndIncrement), defaultUserAgent, 1, true
    )
  }
  /*    https://en.bitcoin.it/wiki/Protocol_documentation#Network_address
   (12 bytes 00 00 00 00 00 00 00 00 00 00 FF FF, followed by the 4 bytes of the IPv4 address). 
   time is 4 bytes (uint32) for version >= 31402, not present in version message */
  case class NetAddrPayload(time:UInt32, services:UInt64, address:Array[Byte], port:UInt16)(isVersionMsg:Boolean) extends {
    val isIPv6:Boolean = address.size == 16
    val addressBytes:Seq[Byte] = if (isIPv6) address else ipv4to6prefix ++ address 
    val timeBytes:Seq[Byte] = (if (isVersionMsg) Nil else time.bytes)
  } with Payload(timeBytes ++ services.uint64.bytes ++ addressBytes ++ port.bytes) {
    def this(address:Array[Byte], port:Int, isVersionMsg:Boolean) = this(getTime, BigInt(0), address, port)(isVersionMsg)
    def this(address:Array[Byte], port:Int) = this(getTime, BigInt(0), address, port)(false)
    if (address.size != 4 && address.size != 16) throw new Exception("Invalid bytes for address: "+address.encodeHex)
  }
}
