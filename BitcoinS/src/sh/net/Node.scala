package sh.net

import akka.actor.PoisonPill
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
  // below methods need to be sync
  def pushTx(tx:Tx):String = await[String](peerGroup ? ("pushtx", tx))
  
  def getBlock(hash:String):Blk = await[Blk](peerGroup ? ("getblock", hash))
 
  def getPeers = await[Array[String]](peerGroup ? "getpeers")
   
  // Below methods make blocking calls to PeerGroup (Actor). 
  // these can result in blocking the application. Left only for testing. 
  @deprecated("Use method ending in Async", "15 Jan 2018")
  def start(relay:Boolean = true) = seeds.map(connectTo(_, relay))
  
  @deprecated("Use method ending in Async", "15 Jan 2018")
  def stop = await[String](peerGroup ? "stop")
  
  @deprecated("Use method ending in Async", "15 Jan 2018")
  def connectTo(hostName:String, relay:Boolean = true):String = await[String](peerGroup ? ("connect", hostName, relay))
    
  @deprecated("Use method ending in Async", "15 Jan 2018")
  def disconnectFrom(hostName:String):String = await[String](peerGroup ? ("disconnect", hostName))
  

  // Use below methods only (they make async calls to PeerGroup)

  def startAsync(relay:Boolean = true) = seeds.map(connectToAsync(_, relay))

  def stopAsync = peerGroup ! "stopAsync"
    
  def connectToAsync(hostName:String, relay:Boolean = true) = peerGroup ! ("connectAsync", hostName, relay)
  
  def disconnectFromAsync(hostName:String) = peerGroup ! ("disconnectAsync", hostName)

  sys.addShutdownHook{
    try {
      peerGroup ! "stopAsync"
      peerGroup ! PoisonPill
    } catch {case a:Throwable => a.printStackTrace}  
  }
}
