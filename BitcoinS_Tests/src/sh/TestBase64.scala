
package sh

import sh.util.Base64

object TestBase64 extends App {
  val s = "Hello"
  val a = Base64.encodeBytes(s.getBytes)
  require(new String(Base64.decode(a)) == s)
  1 to 1000 foreach{i => 
    val n = scala.util.Random.nextInt(1000).abs
    val in = scala.util.Random.nextString(n)
    val out = Base64.encodeBytes(in.getBytes)
    require(in == new String(Base64.decode(out)))
  }
  println("All Base64 test passed")
}