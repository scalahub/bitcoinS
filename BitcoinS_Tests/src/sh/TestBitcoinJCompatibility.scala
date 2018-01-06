package sh

import java.math.BigInteger
import org.bitcoinj.core.ECKey
import org.bitcoinj.params._
import sh.btc.BitcoinS._
import sh.btc.BitcoinS._
import sh.btc._
import sh.ecc._
import sh.ecc.Util._

object TestBitcoinJCompatibility extends App {
  def hash(b:Array[Byte]) = {
    val d = java.security.MessageDigest.getInstance("SHA-256")
    d.digest(b)
  }
  val jNetParam = getJNetParams
  Seq(false, true).map{compressed => 
    1 to 100 foreach{i =>    
      val s = i.toString
      val h = hash(s.getBytes)
      val int = new BigInteger(h)
      val sKey = getKeyFromInt((int mod n).bigInteger, compressed)
      val jKey = ECKey.fromPrivate(int, compressed)
      val sAddr = sKey.pubKey.address
      val jAddr = jKey.toAddress(jNetParam).toString
      assert(sKey.pubKey.eccPubKey.hex.toLowerCase == jKey.getPublicKeyAsHex.toLowerCase, s"Key mismatch for int $int")
      assert(sAddr == jAddr, sAddr+" != "+jAddr)
      print(".")
    }
  }
  println
  println("All BitcoinJ private key from int compatibility tests passed")
  def getKeyFromInt(int: java.math.BigInteger, compressed:Boolean) = new PrvKey_P2PKH(ECCPrvKey(int, compressed), isMainNet)
  def getJNetParams = if (isMainNet) new MainNetParams else new TestNet3Params
}
