
package sh

import sh.util.BytesUtil._
import scala.util.Random

object TestBytesToBits extends App{
  1 to 1000 foreach{i =>
    val b = (1 to 3000).map{j =>
      Random.nextInt.toByte
    }.toArray
    print(".")
    assert(getBitsTest(b) zip b.getBits forall{case (l, r) => l == r})    
  }
  
  def getBitsTest(bytes:Array[Byte]) = { // original 
    val hex = bytes.encodeHex
    val bits = BigInt(hex, 16).toString(2)
    (Array.fill(bytes.size * 8 - bits.size)('0') ++ bits).map{
      case '1' => true
      case '0' => false
    }
  }   
  
  println
  println("passed")
}
