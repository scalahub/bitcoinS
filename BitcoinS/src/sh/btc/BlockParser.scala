package sh.btc

import BitcoinUtil._
import sh.btc.DataStructures._

class BlockParser(bytes:Array[Byte]) extends TxParserSegWit (bytes) { 
  // Header is the first 80 bytes, which has following data:
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
  // IMPORTANT: Do not access both txs and segWitTxs for the same block. 
  // (Since both iterate over the byte array, so calling one after the other will give wrong results)
  // If needed both, then instantiate a new instance of BlockParser and use it.
  private var isParsed = false
  lazy val txs:Seq[TxSimple] = getTxs
  lazy val segWitTxs:Seq[TxSegWit] = getSegWitTxs
  
  // https://bitcoin.org/en/developer-reference#raw-transaction-format        
  private def getTxs:Seq[TxSimple] = {
    if (isParsed) throw new Exception("Block already parsed.")  
    val txs = 1 to getCompactInt map (_ => getTx) // first getCompactInt returns numTx
    isParsed = true
    txs
  }

  private def getSegWitTxs:Seq[TxSegWit] = {
    if (isParsed) throw new Exception("Block already parsed.")  
    val txs = 1 to getCompactInt map (_ => getSegWitTx) // first getCompactInt returns numTx
    isParsed = true
    txs
  }  
}
