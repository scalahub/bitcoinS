
package sh.net

import sh.net.DataStructures._
import sh.net.Parsers.MsgParser
import sh.net.Peer.debug
import sh.net.NetUtil._

class DataProcessor {
  var incompletePacket:Array[Byte] = Array()
  def getCommands(initialBytes:Array[Byte]) = {
    var optParser:Option[MsgParser] = Some(new MsgParser(initialBytes)) // initialize the parser with initial bytes
    var validMsgs:Seq[(String, Array[Byte])] = Nil // (command, payload bytes) // this will be returned after all bytes are consumed
    while (optParser.isDefined) { // repeately parse the data, each parse return at most ONE header and at most ONE payload for that header
      optParser.map{parser =>
        optParser = None // initially set to None for next iteration. We will set it to Some(...) when needed below
        parser.result.map{
          case (header, Some(payload)) => 
            incompletePacket = Array() // if we have a complete payload, reset incompletePacket to empty
            validMsgs :+= (header.command, payload) 
            val unparsed = parser.bytes.drop(headerLen + header.payloadLen)
            if (unparsed.nonEmpty) optParser = Some(new MsgParser(unparsed)) // if there are enough bytes for header then parse remaining bytes            
            if (debug) println(s"[-] checksum[${header.checkSumHex}] payload[${header.payloadLen}] command[${header.command}] parsed[${parser.bytes.size}] initial[${initialBytes.size}]")
            /* if above requirement fails, it implies that unparsed data is non-empty but less than 24 bytes.. i.e. the original packet was of the form:
               |header|payload|header|payload|hea--- (i.e., incomplete header too. This is not expected, but if it occurs, we need to handle it,
               (perhaps by adding the unparsed bytes to incompletePacket.) */
          case (header, None) => // header is defined, no payload and invalid size            
            incompletePacket = parser.bytes
            if (debug) println(s"[+] checksum[${header.checkSumHex}] payload[${header.payloadLen}] command[${header.command}] parsed[${parser.bytes.size}] initial[${initialBytes.size}]")
        }.getOrElse{ // neither header nor payload. Must be data
          if (incompletePacket.nonEmpty) {
            incompletePacket ++= parser.bytes
            optParser = Some(new MsgParser(incompletePacket))
            incompletePacket = Array() // reset incomplete packet
          } else incompletePacket = parser.bytes
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
