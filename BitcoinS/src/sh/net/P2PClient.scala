
package sh.net

import akka.actor.{ Actor, ActorRef, Props }
import akka.io.{ IO, Tcp }
import akka.util.ByteString
import java.net.InetSocketAddress
import akka.io.{ IO, Tcp }
import Tcp._
import sh.net.DataStructures._

/* code borrowed from https://doc.akka.io/docs/akka/2.5.8/io-tcp.html?language=scala#connecting */
private object P2PClient {
  def props(remote: InetSocketAddress, replies: ActorRef, magicBytes:Array[Byte]) = 
    Props(classOf[P2PClient], remote, replies, magicBytes:Array[Byte])
}

// this actor talks to remote p2p peer
private class P2PClient(remote: InetSocketAddress, listener: ActorRef, magicBytes:Array[Byte]) extends Actor {

  import context.system

  IO(Tcp) ! Connect(remote)

  def receive = {
    case CommandFailed(_: Connect) ⇒
      listener ! "connect failed"
      context stop self

    case c @ Connected(remote, local) ⇒
      listener ! c
      val connection = sender()
      connection ! Register(self)
      context become {
        case m:P2PMsg => 
          connection ! Write(ByteString(magicBytes ++ m.bytes)) // need to add magic bytes
        
        case CommandFailed(w: Write) ⇒
          // O/S buffer was full
          listener ! "write failed"
        
        case Received(data) ⇒
          listener ! data
        
        case "close" ⇒
          connection ! Close
        
        case _: ConnectionClosed ⇒
          listener ! "connection closed"
          context stop self

        case any ⇒
          println("P2P unhandled command "+any.getClass+":"+any)

      }
  }
}  
