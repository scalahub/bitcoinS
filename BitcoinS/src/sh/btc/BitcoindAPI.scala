package sh.btc

import org.apache.commons.io.IOUtils
import sh.btc.DataStructures._
import sh.util._
import java.io.StringWriter
import java.net.Authenticator
import java.net.HttpURLConnection
import java.net.PasswordAuthentication
import java.net.URL
import java.util.concurrent.atomic.AtomicLong
import sh.ecc.Util._
import sh.util.BytesUtil._
import sh.util.CommonUtil._
import sh.util.StringUtil._
import sh.util.BigIntUtil._
import Json2XML._

// Basic API for talking to bitcoind, if needed, say to scan blockchain or broadcast tx
class BitcoindAPI(rpcuser:String, rpcpassword:String, rpcHost:String) {
  Authenticator.setDefault(
    new Authenticator {
      override def getPasswordAuthentication:PasswordAuthentication = {
        new PasswordAuthentication (rpcuser, rpcpassword.toCharArray)
      }
    }
  )  
  val ctr = new AtomicLong(0)
  def id = ctr.incrementAndGet
  
  def curl(jsonEncodedString:String) = synchronized {
    val httpcon = new URL(rpcHost).openConnection.asInstanceOf[HttpURLConnection]
    httpcon.setDoOutput(true);
    httpcon.setRequestProperty("Content-Type", "application/json");
    httpcon.setRequestProperty("Accept", "application/json");
    httpcon.setRequestProperty("Connection", "close");      
    httpcon.setRequestMethod("POST");
    httpcon.connect  
    val outputBytes = jsonEncodedString.getBytes("UTF-8");
    using(httpcon.getOutputStream){os =>
      os.write(outputBytes)
    }
    //https://stackoverflow.com/questions/309424/read-convert-an-inputstream-to-a-string
    val code = httpcon.getResponseCode
    val isError = code >= 400 && code <= 500
    val resp = using{
      // using method from here: https://stackoverflow.com/a/5218279/243233
      if (isError) httpcon.getErrorStream else httpcon.getInputStream
    }{is =>
      val writer = new StringWriter;
      IOUtils.copy(is, writer, "UTF-8");
      writer.toString;
    }
    httpcon.disconnect
    if (isError) throw new Exception(s"Resp code $code. Error: ${resp.take(200)}") else resp        
  }

  def curlXML(jsonEncodedString:String) = jsonStringToXML(curl(jsonEncodedString))
  
  def getSoftwareVersion:String = {
    val xml = curlXML(
      s"""{"method":"getnetworkinfo","params":[],"id":$id,"jsonrpc":"1.0"}"""
    )
    (xml \\ "subversion").text
  }
  def estimateSmartFee(conf:Int, economicalMode:Boolean) = {
    val mode = if (economicalMode) "ECONOMICAL" else "CONSERVATIVE"
    val xml = curlXML(
      s"""{"method":"estimatesmartfee","params":[$conf, "$mode"],"id":$id,"jsonrpc":"1.0"}"""
    )
    BigDecimal((xml \ "result" \ "feerate").text)
  }
  def getConfirmations(txHash:String) = {
    val xml = curlXML(
      s"""{"method":"getrawtransaction","params":["$txHash", 1],"id":$id,"jsonrpc":"1.0"}"""
    )
    val txids = (xml \ "result" \ "txid")
    if (txids.isEmpty) 0 else {
      val conf = xml \\ "confirmations"
      if (conf.isEmpty) 0 else {
        conf.text.toInt
      } 
    }
  }
  def pushTx(hex:String) = {
    curlXML(
      s"""{"method":"sendrawtransaction","params":["$hex"],"id":$id,"jsonrpc":"1.0"}"""
    ) 
    "Ok"
  }
  def importAddresses(addresses:Array[String]) = {
    if (addresses.nonEmpty) {
      val json1 = "["+addresses.map{address => 
s"""
{
  "scriptPubKey" : { "address": "$address" },
  "timestamp" : 0,
  "label" : ""
}"""
      }.reduceLeft(_+","+_)+"]"
      val json2 = """{"rescan":false}"""
      val xml = curlXML(
        s"""{"jsonrpc": "1.0", "id":$id, "method": "importmulti", "params": [$json1, $json2] }"""
      )    
      (xml \\ "result").map(_.text).toArray
    } else Array[String]()
  }
  def importAddress(address:String) = {
    val label = address
    curl(
      s"""{"jsonrpc": "1.0", "id":$id, "method": "importaddress", "params": ["$address", "", false] }"""
    )    
    "Ok"
  }
  def testConnection = {
    curl(
      s"""{"method":"getblockchaininfo","params":[],"id":$id,"jsonrpc":"1.0"}"""
    )
  }  
  def getTransaction(txHash:String) = {
    val xml = curlXML(
      s"""{"method":"getrawtransaction","params":["$txHash", 1],"id":$id,"jsonrpc":"1.0"}"""
    )
    Parser.parseTxXML((xml \ "result")(0))
  }

  
  def getBlockSummary(blockHash:String):BlkSummary = {
    val xml = curlXML(
      s"""{"method":"getblock","params":["$blockHash"],"id":$id,"jsonrpc":"1.0"}"""
    )
    val txHashes =  xml \\ "tx" map (_.text)
    val prevBlkHash =  (xml \\ "previousblockhash").text
    val version =  (xml \\ "version").text.toLong
    val time = (xml \\ "time").text.toLong * 1000
    new BlkSummary(blockHash, prevBlkHash, time, version, txHashes)
  }

  def getBlock(blockHash:String):Blk = {
    val xml = curlXML(
      s"""{"method":"getblock","params":["$blockHash", 2],"id":$id,"jsonrpc":"1.0"}"""
    ) \ "result"
    val prevBlkHash =  (xml \ "previousblockhash").text
    val version =  (xml \ "version").text.toLong
    val merkleRoot =  (xml \ "merkleroot").text
    val nBits = (xml \ "bits").text.decodeHex
    val nonce = (xml \ "nonce").text.toLong
    val time = (xml \ "time").text.toLong * 1000
    val txs = (xml \ "tx").map(Parser.parseTxXML)
    val header = BlkHeader(blockHash, prevBlkHash, time, version, merkleRoot, nBits, nonce)
  
    Blk(header, txs)
  }
  
  def getBestBlockHash = {
    val xml = curlXML(
      s"""{"method":"getbestblockhash","params":[],"id":$id,"jsonrpc":"1.0"}"""
    )
    (xml \\ "result").text
  }
  def getAddresses = {
    val xml = curlXML(
      s"""{"method":"getaddressesbyaccount","params":[""],"id":$id,"jsonrpc":"1.0"}"""
    )
    
    val addresses = (xml \\ "result").map(_.text)
    addresses
  }
  
  def createRawTransaction(ins:Array[TxIn], outs:Array[TxOut]) = {
    val in = ins.map{
      case TxIn(txHash, vOut) =>
        s"""{\"txid\":\"$txHash\",\"vout\":$vOut}"""
    }.reduceLeft(_+","+_)
    
    val out = outs.map{
      case TxOut(Some(addr), amtBigInt) =>
        val amtBtc = BitcoinUtil.insertDecimal(amtBigInt)
        s"""\"$addr\":$amtBtc"""
    }.reduceLeft(_+","+_)
    
    val qry = s"""
{
  "method": "createrawtransaction", 
  "params": [
     [$in], {$out}
  ],
  "id":$id,
"jsonrpc":"1.0"}"""
    
    val xml = curlXML(
      qry
    )
    (xml \\ "result").text
  }
}

object Parser {
  def parseTxXML(txXML:scala.xml.Node) = {
    
    val hex = (txXML \ "hex").text // used for Sanity check below. Remove after testing. 
    val tx = new TxParser(hex.decodeHex).getTx

    assert(tx.serialize.encodeHex == hex, s"Computed hex: {tx.serialize.encodeHex}. Bitcoind reported: $hex") 
    // following for testing implementation of parser. Not used otherwise
    val txid  = (txXML \ "txid").text
    val segWitTxHash = (txXML \ "hash").text
    val version  = (txXML \ "version").text.toInt
    val locktime = (txXML \ "locktime").text.toLong
    val size = (txXML \ "size").text.toInt
    val vsize = (txXML \ "vsize").text.toInt

    assert(tx.txid == txid, s"Computed txid: {tx.txid}. Bitcoind reported: $txid")
    assert(tx.segWitTxHash == segWitTxHash, s"Computed txid: {tx.segWitTxHash}. Bitcoind reported: $segWitTxHash")
    assert(segWitTxHash != txid == tx.isSegWit, s"isSegWit: ($segWitTxHash != $txid) != ${tx.isSegWit}")
    assert(version == tx.version, s"Bitcoind version: $version != ${tx.version}")
    assert(locktime == tx.lockTime, s"Bitcoind locktime: $locktime != ${tx.lockTime}")
    assert(size == tx.size, s"Bitcoind size: $size != {tx.size}")
    assert(vsize == tx.vSize, s"Bitcoind vsize: $vsize != {tx.vSize}")

    tx
  }  
}

