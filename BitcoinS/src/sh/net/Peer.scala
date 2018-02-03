
package sh.net

import sh.btc.BitcoinUtil._
import java.util.concurrent.atomic.AtomicInteger
import sh.btc.BitcoinS._
import sh.util.StringUtil._
import sh.net.DataStructures._
import sh.net.NetUtil._
import sh.util.BytesUtil._
import sh.btc.BlockParser
import sh.btc.DataStructures._
import sh.btc._
import sh.util.HashUtil._

import akka.actor.{ Actor, Props , ActorRef}
import akka.util.ByteString
import java.net.InetSocketAddress
import akka.io.{ Tcp }
import Tcp._
import akka.actor.ActorSystem

import Parsers._
import DataStructures._
import Payloads._

object Peer {
  val port = if (isMainNet) 8333 else 18333
  val system = ActorSystem("BTCPeerSystem")
  var debug = false // default to false

  def props(peerGroup:ActorRef, relay:Boolean) = Props(classOf[Peer], peerGroup, relay)
  
  val actCtr = new AtomicInteger(0)
  def connectTo(hostName:String, peerGroup: ActorRef, relay:Boolean) = {
    val ctr = actCtr.getAndIncrement
    val peer = system.actorOf(props(peerGroup, relay), name = s"peer_$hostName$ctr")
    val remote = system.actorOf(P2PClient.props(new InetSocketAddress(hostName, port), peer), s"peer-client_$hostName$ctr")
    peer ! remote  // Send remote address to peer. It will store for internal use
    peer // return an actor ref to the peer
  }
}

class Peer(peerGroup:ActorRef, relay:Boolean) extends Actor {

  var optClientActor:Option[ActorRef] = None // reference to the actor that talks to remote peer
  
  var optRemoteAddr:Option[InetSocketAddress] = None // address of remote peer
  var optLocalAddr:Option[InetSocketAddress] = None // our local address, needed for ADDR messages
  
  lazy val peer = optRemoteAddr.map(_.toString).getOrElse("none")  // string representation of remote peer for debug display
  
  val dataProcessor = new DataProcessor
  def receive = {
    case data:ByteString => // data packet received from remote peer (INV/Tx/Block/Version/etc)
      dataProcessor.getCommands(data).foreach(processCommand) // process the data (parse it and obtain the commands in the data)

    case a:ActorRef => optClientActor = Some(a) // received actorRef of remote.     
    
    case m:P2PMsg => optClientActor.map(_ ! m)  // push tx, get tx/block etc (commands coming fromg PeerGroup)

    case Connected(remote, local) => // connect message send initially (or during reconnect, once implemented)
      optRemoteAddr = Some(remote) 
      optLocalAddr = Some(local)
      sender ! VersionMsg(local, remote, relay) // send version message
      peerGroup ! ("connected", remote.getHostString)
    case "connection closed" => // remote connection closed
      println(s"Remote connection closed: $peer")
      peerGroup ! "disconnected"
      context.stop(self)
      
    case "stop" => // someone sent this actor a shutdown signal 
      println(s"Peer received stop signal: $peer")
      optClientActor.map(_ ! "close")  // ask client to disconnect
    
    case any => println(s"Peer got unknown message: [$any] from [$sender]")
  }
  
  def sendAddr = optLocalAddr.map(inetAddr => sender ! new AddrPayload(inetAddr))

  def processCommand(commandBytes:(String, ByteString)):Unit = {
    val (command, byteString) = commandBytes
    val bytes = byteString.toArray
    command match {
      case `getDataCmd` => peerGroup ! (getDataCmd, new InvPayloadParser(bytes).inv)
      case `rejectCmd` => peerGroup ! new RejectPayloadParser(bytes).rej
      case `invCmd` => peerGroup ! new InvPayloadParser(bytes).inv
      case `notFoundCmd` => peerGroup ! (notFoundCmd, new InvPayloadParser(bytes).inv)
      case `getAddrCmd` => sendAddr // if needed to parse payload: val addr = new AddrPayloadParser(bytes).addr // do something with addr 
      case `addrCmd` => peerGroup ! new AddrPayloadParser(bytes).addr 
        sendAddr
      case `versionCmd` => if (new VersionPayloadParser(bytes).version.version.int32 >= ourVersion) sender ! VerAckMsg 
      case `pingCmd` => sender ! PongMsg(new PingPayloadParser(bytes).ping)
      case `verAckCmd` => sendAddr
      case `txCmd` => peerGroup ! new TxParser(bytes.toArray).getTx
      case `blockCmd` => peerGroup ! new BlockParser(bytes.toArray).getBlock 
      case `alertCmd` => // ignore 
      case cmd => println(s"Unhandled command $cmd") // do nothing for now        
    }
  }
}

