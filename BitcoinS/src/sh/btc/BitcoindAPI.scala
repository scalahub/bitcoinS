package sh.btc

import sh.btc.DataStructures._
import sh.util._
import java.net.Authenticator
import java.net.PasswordAuthentication
import java.util.concurrent.atomic.AtomicLong
import sh.ecc.Util._
import sh.util.BytesUtil._
import sh.util.StringUtil._
import sh.util.BigIntUtil._

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
  
  def getSoftwareVersion:String = {
    val xml = CurlJsonData.curlXML(
      rpcHost, 
      s"""{"method":"getnetworkinfo","params":[],"id":$id,"jsonrpc":"1.0"}"""
    )
    (xml \\ "subversion").text
  }
  def getConfirmations(txHash:String) = {
    val xml = CurlJsonData.curlXML(
      rpcHost, 
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
    CurlJsonData.curlXML(
      rpcHost, 
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
      val xml = CurlJsonData.curlXML(
        rpcHost, 
        s"""{"jsonrpc": "1.0", "id":$id, "method": "importmulti", "params": [$json1, $json2] }"""
      )    
      (xml \\ "result").map(_.text).toArray
    } else Array[String]()
  }
  def importAddress(address:String) = {
    val label = address
    CurlJsonData.curl(
      rpcHost, 
      s"""{"jsonrpc": "1.0", "id":$id, "method": "importaddress", "params": ["$address", "", false] }"""
    )    
    "Ok"
  }
  def testConnection = {
    CurlJsonData.curl(
      rpcHost, 
      s"""{"method":"getblockchaininfo","params":[],"id":$id,"jsonrpc":"1.0"}"""
    )
  }  
  def getTransaction(txHash:String) = {
    val xml = CurlJsonData.curlXML(
      rpcHost, 
      s"""{"method":"getrawtransaction","params":["$txHash", 1],"id":$id,"jsonrpc":"1.0"}"""
    )
    Parser.parseTxXML((xml \ "result")(0))
  }

  
  def getBlockSummary(blockHash:String):BlkSummary = {
    val xml = CurlJsonData.curlXML(
      rpcHost, 
      s"""{"method":"getblock","params":["$blockHash"],"id":$id,"jsonrpc":"1.0"}"""
    )
    val txHashes =  xml \\ "tx" map (_.text)
    val prevBlkHash =  (xml \\ "previousblockhash").text
    val version =  (xml \\ "version").text.toLong
    val time = (xml \\ "time").text.toLong * 1000
    new BlkSummary(blockHash, prevBlkHash, time, version, txHashes)
  }

  def getBlock(blockHash:String):Blk = {
    val xml = CurlJsonData.curlXML(
      rpcHost, 
      s"""{"method":"getblock","params":["$blockHash", 2],"id":$id,"jsonrpc":"1.0"}"""
    )
    val prevBlkHash =  (xml \\ "previousblockhash").text
    val version =  (xml \\ "version").text.toLong
    val merkleRoot =  (xml \\ "merkleroot").text
    val nBits = (xml \\ "bits").text.decodeHex
    val nonce = (xml \\ "nonce").text.toLong
    val time = (xml \\ "time").text.toLong * 1000
    val txs = (xml \\ "result" \\ "tx").map(Parser.parseTxXML)
    Blk(blockHash, prevBlkHash, time, version, txs, merkleRoot, nBits, nonce)
  }
  
  def getBestBlockHash = {
    val xml = CurlJsonData.curlXML(
      rpcHost, 
      s"""{"method":"getbestblockhash","params":[],"id":$id,"jsonrpc":"1.0"}"""
    )
    (xml \\ "result").text
  }
  def getAddresses = {
    val xml = CurlJsonData.curlXML(
      rpcHost, 
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
    
    val xml = CurlJsonData.curlXML(
      rpcHost, 
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
  def parseTxXML_Orig(txXML:scala.xml.Node) = {
    val txhash  = (txXML \ "txid").text
    val version  = (txXML \ "version").text.toInt
    val segWitHash = (txXML \ "hash").text
    val locktime = (txXML \ "locktime").text.toLong
    val size = (txXML \ "size").text.toInt
    val vsize = (txXML \ "vsize").text.toInt
    val hex = (txXML \ "hex").text // used for Sanity check below. Remove after testing. 
    val vOuts = (txXML \ "vout").map{vOut =>
      val value = BigInt(BitcoinUtil.removeDecimal((vOut \ "value").text))
      val n = ((vOut \ "n").text).toInt
      val sp = (vOut \ "scriptPubKey")
      val outtype =  (sp \ "type").text      
      val optAddress = if (outtype == "pubkeyhash") { // p2pkh
        val reqSigs = (sp \ "reqSigs").text.toInt
        if (reqSigs == 1) { // single signature
          val address = (sp \ "addresses").text
          Some(address)
        } else None
      } else None
      TxOut(optAddress, value)
    }
    
    val (vIns, vWits) = (txXML \ "vin").map{vIn =>       
      if ((vIn \ "txid").nonEmpty) {
        val wits = (vIn \ "txinwitness").map{w =>
          w.text.decodeHex.toSeq
        }
        Some((TxIn((vIn \ "txid").text, ((vIn \ "vout").text).toInt), TxWit(wits)))        
      } else None
    }.collect{
      case Some((in, wit)) => (in, wit)
    }.unzip
    val tx = Tx(version, vIns, vOuts, vWits.toArray, locktime, txhash, segWitHash != txhash, segWitHash, size, vsize)
    // Sanity check below. Remove after testing
    assert(hex == tx.serialize.encodeHex, s"Found ${tx.serialize.encodeHex}. Expected $hex")
    Tx(version, vIns, vOuts, vWits.toArray, locktime, txhash, segWitHash != txhash, segWitHash, size, vsize)
  }  
}

