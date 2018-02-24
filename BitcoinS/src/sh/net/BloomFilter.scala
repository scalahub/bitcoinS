
package sh.net

import sh.util.StringUtil._
import sh.util.Base58Check
import java.util.concurrent.atomic.AtomicInteger
import scala.util.hashing.MurmurHash3
import sh.btc._
import sh.net.NetUtil._
import sh.util.BigIntUtil._

object BloomFilter{
  val BYTES_MAX = 36000
  val FUNCS_MAX = 50

  val nFlags = 0

  val log2 = math.log(2)
  val log2sqr = log2 * log2
  def getFilter(maxItems:Int, falsePositive:Double):BloomFilter = {
    getFilter(maxItems, falsePositive, scala.util.Random.nextInt.abs)
  }
  def getFilter(maxItems:Int, falsePositive:Double, nTweak:Long):BloomFilter = {
    getFilter(maxItems, falsePositive, nTweak, 0)
  }
  def getFilter(maxItems:Int, falsePositive:Double, nTweak:Long, nFlags:Int) = {
    require(falsePositive < 1)
    require(falsePositive > 0)
    require(maxItems > 0)
    
    val nFilterBytes = (-math.log(falsePositive)*maxItems/log2sqr/8).min(BYTES_MAX).toInt
    
    val nHashFuncs = (nFilterBytes * 8 / maxItems * log2).min(FUNCS_MAX).toInt
    new BloomFilter(nTweak, nHashFuncs, nFlags, nFilterBytes)
  }
}
class BloomFilter private[sh] (val nTweak:UInt32, val nHashFuncs:UInt32, val nFlags:UInt8, val nFilterBytes:Int) {
  /* https://en.wikipedia.org/wiki/Bloom_filter
An empty Bloom filter is a bit array of m bits, all set to 0. 
There must also be k different hash functions defined, 
each of which maps or hashes some set element to one of the m array positions, 
generating a uniform random distribution. 
Typically, k is a constant, much smaller than m, 
which is proportional to the number of elements to be added; 
the precise choice of k and the constant of proportionality of m are determined by the intended false positive rate of the filter.

To add an element, feed it to each of the k hash functions to get k array positions. 
Set the bits at all these positions to 1.
   */
  val ctr = new AtomicInteger(0)
  if (nFilterBytes > 36000) throw new Exception("Filter size must be <= 36000")
  if (nFilterBytes < 1) throw new Exception("Filter size must be > 0")
  val numBits = nFilterBytes * 8
  val filter = Array.fill[Boolean](numBits)(false)
  def mod(int:Int) = { 
    // from https://github.com/bitcoinj/bitcoinj/blob/3177bd52a2bfa491c5902e95b8840030e1a31159/core/src/main/java/org/bitcoinj/core/BloomFilter.java
    ((int&0xFFFFFFFFL) % numBits).toInt
  }
  val const = BigInt("FBA4C795", 16).toLong
  val hasheSeeds = 1 to nHashFuncs map(i => (i - 1) * const + nTweak)
  val hashes = hasheSeeds.map{seed => s:Array[Byte] => mod(MurmurHash3.bytesHash(s, seed.toInt)) }

  def add(bytes:Array[Byte]) = {
    hashes.map(_(bytes)).foreach(i => filter(i) = true)
  }
  
  def exists(bytes:Array[Byte]) = hashes.map(_(bytes)).forall(filter)  
   
  def getActualFalsePositiveRate(numItems:Int) = {
    math.pow(1 - math.exp(-(nHashFuncs*numItems.toDouble/numBits.toDouble)), nHashFuncs.uint32)
    // (1-exp(-(kn/m)))^k // from wiki/bip37
  }
  def filterBitString = filter.map{
    case true => '1'
    case _ => '0'
  }.mkString
  
  def addAddress(address:String) = {
    if (BitcoinS.isValidAddress(address)){
      val decoded = Base58Check.decode(address)             
      val (prefix, pubKeyHashOrData) = (decoded(0), decoded.drop(1))  // first byte is network version and address type
      add(pubKeyHashOrData)
    } else throw new Exception("Invalid address")
  }

  def addPubKey(pubKey:PubKey) = {
    add(pubKey.bytes)
    addAddress(pubKey.address)
  }

  def addTx(txid:String, vOut:Int) = {
    val vOutInt32:UInt32 = vOut
    val char32:Char32 = txid
    add(char32.bytes.toArray ++ vOutInt32.bytes)
  }
  
  def bytes = filterBitString.reverse.grouped(8).map{byteString =>
    BigInt(byteString, 2).toBytes 
  }.flatten.toArray.reverse
}
