package sh.net

import akka.actor.PoisonPill
import akka.util.Timeout
import scala.concurrent.{Await, Future}
import sh.btc.DataStructures._
import akka.actor.ActorSystem
import sh.btc.TxParser
import sh.util.StringUtil._

trait Node extends EventListener {
  val id:String // BTC / BCH    
  val seeds:Seq[String] // the default network nodes
  val version:Int  
  val userAgent:String    
  val serviceBit:Int  
  val magicBytes:Array[Byte]
  /*  Network   Magic value	Sent over wire as
      main      0xD9B4BEF9	F9 BE B4 D9
      testnet   0xDAB5BFFA	FA BF B5 DA 
      testnet3	0x0709110B	0B 11 09 07
      namecoin	0xFEB4BEF9	F9 BE B4 FE   
      bitcoinABC  e3:e1:f3:e8   */   
  val system = ActorSystem(s"PeerGroup$id")  

  val peerGroup = system.actorOf(
    PeerGroup.props(
      this, PeerGroupConfig(version, userAgent, serviceBit, magicBytes)
    ), name = s"PeerGroup$id"
  )

  import scala.concurrent.duration._
  import akka.pattern.ask 
  implicit val timeout = Timeout(30 seconds)
  /* https://alvinalexander.com/scala/akka-actor-how-to-send-message-wait-for-reply-ask
   * https://groups.google.com/forum/#!topic/play-framework/a8Lh3v7jAZw */
  private def await[T](future:Future[Any]) = Await.result(future, timeout.duration).asInstanceOf[T]

  // below commands to be exposed 
  // Below methods make blocking calls to PeerGroup (Actor). These can result in blocking the application.
  def pushTx(tx:Tx):String = await[String](peerGroup ? ("pushtx", tx))
  
  def pushTx(hex:String):String = pushTx(new TxParser(hex.decodeHex).getTx)
  
  def getBlock(hash:String):Blk = await[Blk](peerGroup ? ("getblock", hash))
 
  def getPeers = await[Array[String]](peerGroup ? "getpeers")
   
  @deprecated("Use method ending in Async", "15 Jan 2018")
  def connectToAllSeeds(relay:Boolean = true) = seeds.map(connectTo(_, relay))
  
  @deprecated("Use method ending in Async", "15 Jan 2018")
  def disconnectFromAll = await[String](peerGroup ? "stop")
  
  @deprecated("Use method ending in Async", "15 Jan 2018")
  def connectTo(hostName:String, relay:Boolean = true):String = await[String](peerGroup ? ("connect", hostName, relay))
    
  @deprecated("Use method ending in Async", "15 Jan 2018")
  def disconnectFrom(hostName:String):String = await[String](peerGroup ? ("disconnect", hostName))
  
  @deprecated("Use method ending in Async", "15 Jan 2018")
  def addFilter(f:BloomFilter):Boolean = await[Boolean](peerGroup ? f)
    
  // Use below methods only (they make async calls to PeerGroup)

  def connectToAllSeedsAsync(relay:Boolean = true) = seeds.map(connectToAsync(_, relay))

  def disconnectFromAllAsync = peerGroup ! "stopAsync"
    
  def connectToAsync(hostName:String, relay:Boolean = true) = peerGroup ! ("connectAsync", hostName, relay)
  
  def disconnectFromAsync(hostName:String) = peerGroup ! ("disconnectAsync", hostName)

  sys.addShutdownHook{
    try {
      peerGroup ! "stopAsync"
      peerGroup ! PoisonPill
    } catch {case a:Throwable => a.printStackTrace}  
  }
}
