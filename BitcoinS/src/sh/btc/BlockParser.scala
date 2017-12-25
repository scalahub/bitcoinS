package sh.btc

import BitcoinUtil._
import sh.btc.DataStructures._

class BlockParser(bytes:Array[Byte]) extends TxParserSegWit (bytes) { 
  //private val header = getNextBytes(80) // 80 bytes for header
  val version = getNext4Int
  val prevBlockHash = getNext32Hash
  val merkleRootHash = getNext32Hash  
  val time = getNext4Int
  val nBits = getNext4Int
  val nonce = getNext4Int
  lazy val currBlockHash = getHashed(getBytes(0, 79)) 
  
  if (debug) {
    println(s"Current block hash: $currBlockHash")
    println(s"Previous block hash: $prevBlockHash")
  }
  // if header not needed, replace above by: incrCtr(80) // skip 80 bytes of header
  lazy val txs:Seq[TxSimple] = getTxs
  lazy val segWitTxs:Seq[TxSegWit] = getSegWitTxs
  
  // https://bitcoin.org/en/developer-reference#raw-transaction-format        
  private def getTxs:Seq[TxSimple] = 1 to getCompactInt map (_ => getTx) // first getCompactInt returns numTx

  private def getSegWitTxs:Seq[TxSegWit] = 1 to getCompactInt map (_ => getSegWitTx)
  
}
