
package sh.net

import sh.btc.BitcoinUtil._
import sh.btc.BitcoinS._
import sh.util.StringUtil._
import sh.net.DataStructures._
import sh.net.NetUtil._
import sh.util.BytesUtil._
import sh.btc.BlockParser
import sh.btc.DataStructures.Tx
import sh.btc.TxParser
import sh.ecc.Util._
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
  def connectTo(hostName:String) = {
    val peer = system.actorOf(Props[Peer], name = s"peer_$hostName")
    val remote = system.actorOf(P2PClientActor.props(new InetSocketAddress(hostName, port), peer), s"peer-client_$hostName")
    peer ! remote  // Send remote address to peer. It will store for internal use
    peer // return an actor ref to the peer
  }
  var debug = false // default to false

  var pushedTx = Map[String, Tx]() // txid -> tx // will need to periodically clear
  
  var seenTx = Map[String, Tx]() // txid -> tx // txs seen by us AND network
  var unknownTxHashes = Set[String]() // txid // txs seen by network byt not by us. Will store the txids received from INV messages
  
  var seenBlk = Set[String]() // blockhash // block hashes seen by us
  
}

class Peer extends Actor {
  import Peer._
  
  var optClientActor:Option[ActorRef] = None // reference to the actor that talks to remote peer
  var optRemoteAddr:Option[InetSocketAddress] = None // address of remote peer
  var optLocalAddr:Option[InetSocketAddress] = None // our local address, needed for ADDR messages
  
  lazy val peer = optRemoteAddr.map(_.toString).getOrElse("none")  // string representation of remote peer for debug display
  
  val dataProcessor = new DataProcessor
  def receive = {
    case a:ActorRef => optClientActor = Some(a) // received actor ref of remote.     
    case tx:Tx =>  // tx to broadcase 
      pushedTx += tx.txid -> tx // add to push tx
      optClientActor.map(_ ! PushTxInvMsg(tx.txid))  // send inv msg about the tx
    case data:ByteString => // data packet received from remote peer (INV/Tx/Block/Version/etc)
      dataProcessor.getCommands(data.toArray).foreach(processCommand) // process the data (parse it and obtain the commands in the data)
    case Connected(remote, local) => // connect message send initially (or during reconnect, once implemented)
      optRemoteAddr = Some(remote) 
      optLocalAddr = Some(local)
      sender ! VersionMsg(local, remote) // send version message
    case "stop" | "connection closed" => // someone send this actor a shutdown signal (either remote stopped or manually)
      println(s"Peer stopped: $peer")
      context.stop(self)
    case any => println(s"Peer got unknown message: [$any] from [$sender]")
  }
  
  def processGetDataCmd(inv:InvPayload) = { // other peer request for data
    inv.invVectors.collect{ 
      case InvVector(MSG_TX, char32) => // peer can only request for tx that we originate (i.e. in pushedTx)
        if (debug) println(s"Peer [$peer] asked for txid: "+char32.rpcHash) 
        pushedTx.get(char32.rpcHash).map(tx => sender ! TxMsg(tx)) // send the Tx 
    }    
  }
  
  def processTxCmd(p:Payload) = { // peer has send a tx to us (possibly as a response to our getdata)
    val tx = new TxParser(p.bytes.toArray).getTx // first parse the tx
    if (unknownTxHashes.contains(tx.txid)) { // unknownhashes will contain txid if we have made made a getdata req, as a response to inv command
      seenTx += tx.txid -> tx // add to seen tx
      unknownTxHashes -= tx.txid // remove from unknown tx.. We will not store this again
      // if (debug) println(s"Received tx [${tx.txid}] from [$peer]") 
      // todo: validate tx,  push to other peers and invoke handler onTx
    }
  }
  
  def processBlkCmd(p:Payload) = { // peer has send a tx to us (possibly as a response to our getdata)
    val blk = new BlockParser(p.bytes.toArray).getBlock // parse the block
    seenBlk += blk.hash
    if (debug) println(s"Received block [${blk.hash}] from [$peer]")
  }
  
  def processInvCmd(inv:InvPayload) = { // peer has send us an inv command (possibly for a new block, or a new unconf tx) (which we could have possibly seen)
    val invToSend = inv.invVectors.filter{
      case InvVector(MSG_BLOCK, char32) if seenBlk.contains(char32.rpcHash) => false // this is for a block which we have seen
      case InvVector(MSG_BLOCK, _) => true // this is for a block which we have not seen
      case InvVector(MSG_TX, char32) if pushedTx.contains(char32.rpcHash) => // this is our tx
        pushedTx.get(char32.rpcHash).map(seenTx += char32.rpcHash -> _) // if this is our tx, add to seen tx too (don't remove from pushedTx for now)
        false
      case InvVector(MSG_TX, char32) if seenTx.contains(char32.rpcHash) => false 
      case InvVector(MSG_TX, char32) => 
        unknownTxHashes += char32.rpcHash // we have not seen the tx but may have seen the txid. Add it again anyway to be sure
        true
      case _ => false
    } 
    sender ! GetDataMsg(invToSend) // send the getdata command for the data we need
  }
  
  def sendAddr = optLocalAddr.map(inetAddr => sender ! new AddrPayload(inetAddr))

  def processCommand(commandBytes:(String, Array[Byte])):Unit = {
    val (command, bytes) = commandBytes
    command match {
      case `getDataCmd` => processGetDataCmd(new InvPayloadParser(bytes).inv)
      case `invCmd` => processInvCmd(new InvPayloadParser(bytes).inv)
      case `notFoundCmd` => // if needed to parse payload: val inv = new InvPayloadParser(bytes).inv // do something with inv
      case `addrCmd`| `getAddrCmd` => sendAddr // if needed to parse payload: val addr = new AddrPayloadParser(bytes).addr // do something with addr 
      case `versionCmd` => if (new VersionPayloadParser(bytes).version.version.int32 >= ourVersion) sender ! VerAckMsg 
      case `pingCmd` => sender ! PongMsg(new PingPayloadParser(bytes).ping)
      case `verAckCmd` => sendAddr
      case `txCmd` => processTxCmd(new Payload(bytes))
      case `blockCmd` => processBlkCmd(new Payload(bytes))
      case cmd => println(s"Unhandled command $cmd") // do nothing for now        
    }
  }
}

