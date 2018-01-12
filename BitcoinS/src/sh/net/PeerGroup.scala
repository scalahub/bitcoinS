package sh.net

import sh.btc.DataStructures.{ Blk, Tx }
import sh.net.DataStructures._
import sh.net.Payloads._
import sh.util.StringUtil._
import akka.actor.Status._
import akka.actor.{ Actor, ActorRef, Props, Terminated }
import scala.collection.mutable.{Map => MMap}

/*
 * P2P network design is as follows (refer to the figure below):
 * 
 * Application communicates with an instance of Node (Non Actor). 
 * 
 * Node holds an instance of PeerGroup (Actor)
 * 
 * PeerGroup holds several instance of Peer (Actor)
 * 
 * Each Peer internally holds an instance of P2PClient (Actor) that actually talks to remote 
 * (Note: P2PClient is not shown in the figure but should be implicitly assumed to be inside Peer)
 * 
 *  m is standard Java blocking calls such as m.foo(...)
 *  ! is a non blocking message passing to actor
 *  ? is a blocking message passing to actor
 * 
 * 
 *                                                       |<---!--->  Peer1 <-----tcp----> remotePeer1
 *                                                       |
 *                                                       |<---!--->  Peer2 <-----tcp----> remotePeer2
 *  App  <---m--->  Node  <---!--or--?--->  PeerGroup ---|
 *                                                       |<---!--->  Peer3 <-----tcp----> remotePeer3
 *                                                       |
 *                                                       |<---!--->  Peer4 <-----tcp----> remotePeer4 
 *                                                       |
 *                                                       |...
 *                                                       
 *                                                       
 */

object PeerGroup {
  def props(listener:EventListener) = Props(classOf[PeerGroup], listener)  
}
trait EventListener {
  var onBlkMap:Map[String, Blk => Unit] // listener id, listener
  var onTxMap:Map[String, Tx => Unit] // listener id, listener    
}

import PeerGroup._

class PeerGroup(listener:EventListener) extends Actor {
  
  var peers = MMap[String, ActorRef]()
  
  val pushedTx = MMap[String, Tx]() // txid -> tx // will need to periodically clear
  
  val seenTx = MMap[String, Tx]() // txid -> tx // txs seen by us AND network

  val unknownTxHashes = MMap[String, ActorRef]() // txid -> peer // txs seen by network but not by us. Will store the txids received from INV messages
  // also stores who is the first peer that sent us that inv
  
  val seenBlkHashes = MMap[String, String]() // blk hash => prev blk hash // block hashes seen by us
  
  val getBlockReq = MMap[String, ActorRef]() // blockhash -> local actor (app) that asked for it

  def usingFailure[T](failCondition:Boolean)(failMsg:String)(f: => T):Status = 
    if (failCondition) 
      Failure(new Exception(failMsg)) 
    else 
      try Success(f) catch { case t:Throwable => Failure(t) }

  def usingConnectedAsync[T](f: => T) = usingFailure(peers.isEmpty)("Not connected")(f) match {
    case f@Failure(_) => sender ! f // error, send immediately
    case Success(_) => // do nothing, its an async call. 
  }
  
  def receive = {
    // Below messages are received from a Node object
    case "getpeers" => sender ! peers.keys.toArray // from Node (app made a getpeers req) 
      
    case ("connect", hostName:String, relay:Boolean) => // from Node (app made a connect req)
      sender ! usingFailure(peers.contains(hostName))(s"Already conntected to $hostName"){
        val peer = Peer.connectTo(hostName, self, relay)
        peers += (hostName -> peer)
        // https://doc.akka.io/docs/akka/2.3.4/scala/actors.html#Lifecycle_Monitoring_aka_DeathWatch
        context.watch(peer) // new peerActor created, add to watched peers
        "ok"
      }
      
    case ("disconnect", hostName:String) => // from Node (app made a disconnect req)
      sender ! usingFailure(!peers.contains(hostName))(s"Not conntected to $hostName"){
        peers.get(hostName).map(_ ! "stop")
        "ok"
      }
    
    case ("push", tx:Tx) =>  // from Node (app has send us a "push" tx request (via Node) and we need to send it to others)
      usingConnectedAsync{
        pushedTx += tx.txid -> tx // add to push tx map
        peers.values.foreach(_ ! PushTxInvMsg(tx.txid)) // send to all
        sender ! Success(tx.txid)
      }
        
    case ("getblock", hash:String) => // from Node (app has made a req (via Node) to get a block)
      usingConnectedAsync{
        getBlockReq += hash -> sender
        peers.head._2 ! new GetDataMsg(hash) // send only to first peer as of now
      }
    
    case blk:Blk => // received from REMOTE via a Peer
      /* from a Peer. One of the peers has sent us a block, either as a response to a "getBlock" request issued by Node, 
         or as a resp to a "getblock" request issued by this peerGroupActor (which was resulting from a inv message received from the peer's remote node) */
      if (blk.computeMerkleRoot == blk.merkleRoot) { // validate merkle root      
        // If the claimed tx hashes are indeed in merkle root
        // Note that blk.hash is automatically computed from header, so we don't need to validate it
        // ToDo: other validation.
        //    Tx correctly signed (need UTXO set)
        //    difficulty and height validation
        //    hardcode valid blocks from main chain
        //    
        getBlockReq.get(blk.hash).map{actorRef => // for any pending "getBlock" requests, 
          getBlockReq -= blk.hash // remove from pending "getBlock" requests
          actorRef ! Success(blk) // respond to appropriate caller
        }.getOrElse{
          // we are also maintaining a list of block hashes seen by this PeerGroup
          if (!seenBlkHashes.contains(blk.hash)) { // if we have not seen this hash
            seenBlkHashes += (blk.hash -> blk.prevBlockHash) // add to seenHashes
            listener.onBlkMap.foreach(_._2(blk)) // invoke listeners in Node 
          }
        }
      }

    case tx:Tx => 
      /* from a peerActor (one of peerActor received a tx from its remote counterpart, and it has forwarded it to)
         its peerGroup (this one) for further processing */
      
      if (unknownTxHashes.contains(tx.txid)) { // unknownhashes will contain txid if we have made made a getdata req, as a response to inv command
        unknownTxHashes -= tx.txid // remove from unknown tx.. We will not process this again
        listener.onTxMap.foreach(_._2(tx)) // invoke listeners in Node
        seenTx += tx.txid -> tx // add to seen tx so we don't process it again (note: both seen and unknown must be flushed regularly)
        // todo: validate tx,  push to other peers and invoke handler onTx
      }

    case ("getdata", inv:InvPayload) => // from peerActor (getData request received from remote side)
      inv.invVectors.collect{ 
        case InvVector(MSG_TX, char32) => // as of now, remote can only request for tx that we originate (i.e. in pushedTx)
          pushedTx.get(char32.rpcHash).map{tx => 
            sender ! TxMsg(tx)
          } // send the Tx 
      }    
      
    case inv:InvPayload => // from peerActor (inv from others, declaring new items -- tx or block)
      
      val invToSend = inv.invVectors.filter{
        case InvVector(MSG_BLOCK, char32) if seenBlkHashes.contains(char32.rpcHash) => false // this is for a block which we have seen
        case InvVector(MSG_BLOCK, _) => true // this is for a block which we have not seen. We need it
        case InvVector(MSG_TX, char32) if pushedTx.contains(char32.rpcHash) => // this is our tx
          pushedTx.get(char32.rpcHash).map(seenTx += char32.rpcHash -> _) // if this is our tx, add to seen tx too (don't remove from pushedTx for now)
          false
        case InvVector(MSG_TX, char32) if seenTx.contains(char32.rpcHash) => false 
        case InvVector(MSG_TX, char32) => 
          unknownTxHashes += (char32.rpcHash -> sender) // we have not seen the tx but may have seen the txid. Add it again anyway to be sure
          true
        case _ => false
      } 
      
      if (invToSend.nonEmpty) sender ! GetDataMsg(invToSend) // send the getdata command for the data we need
      
    case Terminated(ref) => // DeathWatch 
      peers = peers.filter{
        case (host, `ref`) => 
          println(s"[watcher] Peer terminated $host")
          false
        case _ => true
      }

    case "stop" =>
      println("Received stop signal")
      peers.foreach(_._2 ! "stop")
      context.stop(self)
      sender ! "Stopping peers and peergroup"
      
    case "disconnect" =>
      println("Received disconnect signal")
      peers.foreach(_._2 ! "stop")
      sender ! "Stopping peers"
    case _ =>
  }
}

