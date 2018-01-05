package sh.btc

import BitcoinUtil._
import sh.btc.DataStructures._
import sh.ecc.Util._
import sh.util.BytesUtil._
import sh.util.StringUtil._
import sh.util.BigIntUtil._
import sh.btc.BitcoinS._

class BlockParser(bytes:Array[Byte]) extends TxParser (bytes) { 
  // Header is the first 80 bytes, which has following data:
  val version = getNext4SInt // signed
  val prevBlockHash = getNext32Hash
  val merkleRootHash = getNext32Hash  
  val time = getNext4UInt // unsigned
  val nBits = getNextBytes(4)
  val nonce = getNext4UInt // unsigned
  val currBlockHash = getHashed(getBytes(0, 79)) 
  if (debug) {
    println(s"Current block hash: $currBlockHash")
    println(s"Previous block hash: $prevBlockHash")
  }
  // if header not needed, replace above by: incrCtr(80) // skip 80 bytes of header
  lazy val txs:Seq[Tx] = 1 to getCompactInt map (_ => getTx) // first getCompactInt returns numTx
  
  // https://bitcoin.org/en/developer-reference#raw-transaction-format        
  lazy val getBlock = BitcoindBlock(
    currBlockHash, prevBlockHash, time, version, txs, txs.map(_.serialize.encodeHex), merkleRootHash, nBits, nonce
  )
}
