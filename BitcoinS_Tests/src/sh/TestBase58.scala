
package sh

import sh.util.Base58Check

object TestBase58 extends App {
  val str = "9b195c66cea;c7b70f9d5d8a47;b0c1e66e85a010;bf339d914a3a;20aff96be11f9"
  val enc = Base58Check.encodePlain(str.getBytes)
  val dec = new String(Base58Check.decodePlain(enc))
  require(str == dec)
  1 to 100 foreach{i => 
    print(".")
    val n = scala.util.Random.nextInt(1000).abs
    val in = scala.util.Random.nextString(n)
    val out = Base58Check.encodePlain(in.getBytes)
    require(in == new String(Base58Check.decodePlain(out)))
  }
  println
  println("Base58 test passed")
}
