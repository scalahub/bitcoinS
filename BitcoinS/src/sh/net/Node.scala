package sh.net

import akka.util.Timeout
import scala.concurrent.{Await, Future}
import sh.btc.DataStructures._
import akka.actor.ActorSystem

trait Node extends EventListener {

  val id:String // BTC / BCH
    
  val seeds:Seq[String] // the default network nodes

  val system = ActorSystem(s"PeerGroup$id")
  
  val peerGroup = system.actorOf(PeerGroup.props(this), name = s"PeerGroup$id")

  var onBlkMap = Map[String, Blk => Unit]() // listenerID -> listener
  
  var onTxMap = Map[String, Tx => Unit]() // listenerID -> listener

  def addOnBlkHandler(id:String, onBlk:Blk => Unit) = onBlkMap += id -> onBlk 
  
  def addOnTxHandler(id:String, onTx:Tx => Unit) = onTxMap += id -> onTx 
  
  import scala.concurrent.duration._
  import akka.pattern.ask 
  implicit val timeout = Timeout(30 seconds)
  /* https://alvinalexander.com/scala/akka-actor-how-to-send-message-wait-for-reply-ask
   * https://groups.google.com/forum/#!topic/play-framework/a8Lh3v7jAZw */
  private def await[T](future:Future[Any]) = Await.result(future, timeout.duration).asInstanceOf[T]

  // below commands to be exposed 
  def start(relay:Boolean = true) = seeds.map(connectTo(_, relay))

  def stop = await[String](peerGroup ? "disconnect")
    
  def connectTo(hostName:String, relay:Boolean = true):String = await[String](peerGroup ? ("connect", hostName, relay))
  
  def disconnectFrom(hostName:String):String = await[String](peerGroup ? ("disconnect", hostName))
  
  def pushTx(tx:Tx):String = await[String](peerGroup ? ("push", tx))
  
  def getBlock(hash:String):Blk = await[Blk](peerGroup ? ("getblock", hash))
  
  def getPeers = await[Array[String]](peerGroup ? "getpeers")
  
  sys.addShutdownHook(try peerGroup ! "stop" catch {case a:Throwable => a.printStackTrace})
}
