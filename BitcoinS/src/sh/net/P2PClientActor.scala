
package sh.net

import akka.actor.{ Actor, ActorRef, Props }
import akka.io.{ IO, Tcp }
import akka.util.ByteString
import java.net.InetSocketAddress
import akka.io.{ IO, Tcp }
import Tcp._
import sh.net.DataStructures._

/* code borrowed from https://doc.akka.io/docs/akka/2.5.8/io-tcp.html?language=scala#connecting */
private object P2PClientActor {
  def props(remote: InetSocketAddress, replies: ActorRef) = Props(classOf[P2PClientActor], remote, replies)
}

private class P2PClientActor(remote: InetSocketAddress, listener: ActorRef) extends Actor {

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
          connection ! Write(ByteString(m.bytes))
        case data: ByteString ⇒
          connection ! Write(data)
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
      }
  }
}  
