
package sh.net

import akka.util.ByteString
import sh.net.DataStructures._
import sh.net.Parsers._
import sh.net.NetUtil._
import sh.net.Peer.debug

class DataProcessor(magicBytes:Array[Byte]) {
  var incompleteByteString:ByteString = ByteString()
  def getCommands(initialBytes:ByteString) = {
    var optParser:Option[MsgParser] = Some(new MsgParser(magicBytes, initialBytes)) // initialize the parser with initial bytes
    var validMsgs:Seq[(String, ByteString)] = Nil // (command, payload bytes) // this will be returned after all bytes are consumed
    while (optParser.isDefined) { // repeately parse the data, each parse return at most ONE header and at most ONE payload for that header
      optParser.map{parser =>
        optParser = None // initially set to None for next iteration. We will set it to Some(...) when needed below
        parser.result.map{
          case (header, Some(payloadBytes)) => 
            incompleteByteString = ByteString() // if we have a complete payload, reset incompletePacket to empty
            validMsgs :+= (header.command, payloadBytes) 
            val unparsed = parser.bytes.drop(headerLen + header.payloadLen)
            if (unparsed.nonEmpty) optParser = Some(new MsgParser(magicBytes, unparsed)) // if there are enough bytes for header then parse remaining bytes            
            if (debug) println(s"[-] checksum[${header.checkSumHex}] payload[${header.payloadLen}] command[${header.command}] parsed[${parser.bytes.size}] initial[${initialBytes.size}]")
          case (header, None) => // header is defined, no payload and invalid size            
            incompleteByteString = parser.bytes
            if (debug) println(s"[+] checksum[${header.checkSumHex}] payload[${header.payloadLen}] command[${header.command}] parsed[${parser.bytes.size}] initial[${initialBytes.size}]")
        }.getOrElse{ // neither header nor payload. Must be data
          if (incompleteByteString.nonEmpty) {
            incompleteByteString ++= parser.bytes
            optParser = Some(new MsgParser(magicBytes, incompleteByteString))
            incompleteByteString = ByteString() // reset incomplete packet
          } else incompleteByteString = parser.bytes
          /* Above 'else' handles the case when the incompletePacket is empty. 
           * This can happen when the parsed bytes are less than 24 (smaller than header) 
           * Some examples: (--- denodes an incomplete item)
           * |header|payload|hea---
           * |hea--- 
           * This is rare but we still need to handle (generally we have the case: |header|payl--- ) */
        }
      }      
    }
    validMsgs
  }
}
