
package sh.net

import sh.util.StringUtil._
import sh.util.BigIntUtil._
import java.util.concurrent.atomic.AtomicInteger
import scala.util.hashing.MurmurHash3
import sh.net.NetUtil._

object BloomFilter{
  val BYTES_MAX = 36000
  val FUNCS_MAX = 50

  val nFlags = 0

  val log2 = math.log(2)
  val log2sqr = log2 * log2
  def getFilter(maxItems:Int, falsePositive:Double):BloomFilter = {
    val nTweak:UInt32 = scala.util.Random.nextInt.abs
    getFilter(maxItems, falsePositive, nTweak)
  }
  def getFilter(maxItems:Int, falsePositive:Double, nTweak:UInt32) = {
    require(falsePositive < 1)
    require(falsePositive > 0)
    require(maxItems > 0)
    BigDecimal(-1)/(BigDecimal(2))
    
    val nFilterBytes = (-math.log(falsePositive)*maxItems/log2sqr/8).min(BYTES_MAX).toInt
    
    val nHashFuncs = (nFilterBytes * 8 / maxItems * log2).min(FUNCS_MAX).toInt
    new BloomFilter(nTweak, nHashFuncs, 0, nFilterBytes)
  }
}
class BloomFilter private[net] (val nTweak:UInt32, val nHashFuncs:UInt32, val nFlags:UInt8, val nFilterBytes:Int) {
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
  def mod(int:Int, n:Int) = {
    val rem = int % n
    if (rem < 0) n + rem else rem
  }
  val const = BigInt("FBA4C795", 16).toLong
  val hasheSeeds = 1 to nHashFuncs map(i => (i - 1) * const + nTweak)
  val hashes = hasheSeeds.map{seed => s:Array[Byte] => mod(MurmurHash3.bytesHash(s, seed.toInt), numBits) }

  def add(bytes:Array[Byte]) = {
    hashes.map(_(bytes)).foreach(i => filter(i) = true)
  }
  
  def exists(bytes:Array[Byte]) = hashes.map(_(bytes)).forall(filter)  
   
  def getActualFalsePositiveRate(numItems:Int) = {
    math.pow(1 - math.exp(-(nHashFuncs*numItems.toDouble/numBits.toDouble)), nHashFuncs.uint32)
    // (1-exp(-(kn/m)))^k
  }
  def filterBitString = filter.map{
    case true => '1'
    case _ => '0'
  }.mkString
  
  def bytes = filterBitString.grouped(8).map{byteString =>
    BigInt(byteString, 2).toBytes 
  }.flatten.toArray
}

object TestFilter extends App {
  /* 
   * https://bitcoin.org/en/developer-examples#creating-a-bloom-filter
   */
  val data = "019f5b01d4195ecbc9398fbf3c3b1fa9bb3183301d7a1fb3bd174fcfa40a2b65".decodeHex
  val data1 = "19f5b01d4195ecbc9398fbf3c3b1fa9bb3183301d7a1fb3bd174fcfa40a2b65".decodeHex
  val f = new BloomFilter(0, 11, 0, 2)
  f.add(data)
  println("Filter: "+f.filterBitString)
  //Filter: 1010110111110000
  //        1010110111110000
  val f1 = BloomFilter.getFilter(100000, 0.01)
  println("Actual rate: "+f1.getActualFalsePositiveRate(100000))
  
  val f2 = BloomFilter.getFilter(1, 0.0001, 0)
  f1.add(data)
  f2.add(data)
  println("Filter: "+f2.filterBitString)
  println("=> "+f1.exists(data))
  println("=> "+f1.exists(data1))
  println("=> "+BigInt("111111111", 2).toBytes.size)
}



