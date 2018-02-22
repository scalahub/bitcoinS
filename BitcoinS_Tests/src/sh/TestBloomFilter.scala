
package sh

import sh.util.StringUtil._
import sh.util.BytesUtil._
import sh.net.Payloads.FilterLoadPayload
import sh.btc._
import sh.ecc.ECCPrvKey
import sh.ecc.ECCPubKey
import sh.net.BloomFilter
import sh.net.NetUtil._
import sh.net.Peer
import sh.util.BigIntUtil._

object TestBloomFilter extends App {
  BitcoinS.isMainNet = false
  val data = "019f5b01d4195ecbc9398fbf3c3b1fa9bb3183301d7a1fb3bd174fcfa40a2b65".decodeHex
  val f = BloomFilter.getFilter(1, 0.0001, 0)
  f.add(data)
  assert(f.filterBitString == "1010110111110000")
  assert(FilterLoadPayload(f).bytes.toArray.encodeHex == "02b50f0b0000000000000000")
  //  02 ......... Filter bytes: 2
  //  b50f ....... Filter: 1010 1101 1111 0000
  //  0b000000 ... nHashFuncs: 11
  //  00000000 ... nTweak: 0/none
  //  00 ......... nFlags: BLOOM_UPDATE_NONE
  TestBloomFilterBitJ
  println("All bloom filter tests passed")
}  

object TestBloomFilterNet extends App { 
  BitcoinS.isMainNet = false
  val f = BloomFilter.getFilter(100, 0.01)
  val address0 = new PrvKey_P2PKH(new ECCPrvKey(BigInt("2345678900786756453545676787856546768"), true), false).pubKey.address
  val address1 = new PrvKey_P2PKH(new ECCPrvKey(BigInt("2345678900746767"), true), false).pubKey.address
  Peer.debug = true
  println(address0)
  println(address1)
  assert(address0 == "mgmFkKscVJSP1RyZv5b32V1t8gvbfWeS1k")  
  assert(address1 == "n1W8PWXAfZNgPbpByj3nat4xuVKx4aUetj")  // won't match this address
  BitcoinSNode.connectTo("localhost", false)
  f.addAddress(address0)
  BitcoinSNode.addOnTxHandler("1", tx => {
      tx.outs.foreach{out =>
        println("ONTX to address: "+out.optAddress.getOrElse("None"))
      }
    }
  )
  
  Thread.sleep(1000)
  BitcoinSNode.addFilter(f)
  println("added filter")
}

object TestBloomFilterBitJ {
  // https://github.com/bitcoinj/bitcoinj/blob/a7cad0ede447d4bcba7dd55639df442a408df6fb/core/src/test/java/org/bitcoinj/core/BloomFilterTest.java
  val f = BloomFilter.getFilter(3 /* nItems */, 0.01, 0 /* nTweak */, 1 /* nFlags */)
  f.add("99108ad8ed9bb6274d3980bab5a85c048f0950c8".decodeHex)
  f.add("b5a2c786d9ef4658287ced5914b37a1b4aa32eee".decodeHex)
  f.add("b9300670b4c5366e95b2699e8b18bc75e5f729c5".decodeHex)
  assert(f.exists("99108ad8ed9bb6274d3980bab5a85c048f0950c8".decodeHex))
  assert(!f.exists("19108ad8ed9bb6274d3980bab5a85c048f0950c8".decodeHex))
  assert(f.exists("b5a2c786d9ef4658287ced5914b37a1b4aa32eee".decodeHex))
  assert(f.exists("b9300670b4c5366e95b2699e8b18bc75e5f729c5".decodeHex))
  assert(FilterLoadPayload(f).bytes.toArray.encodeHex == "03614e9b050000000000000001")
  TestBloomFilterBitJ2
}

object TestBloomFilterBitJ2 {
  // https://github.com/bitcoinj/bitcoinj/blob/a7cad0ede447d4bcba7dd55639df442a408df6fb/core/src/test/java/org/bitcoinj/core/BloomFilterTest.java
  val f = BloomFilter.getFilter(3 /* nItems */, 0.01, 2147483649L /* nTweak */, 2 /* nFlags = UPDATE_P2PUBKEY_ONLY */)
  f.add("99108ad8ed9bb6274d3980bab5a85c048f0950c8".decodeHex)
  f.add("b5a2c786d9ef4658287ced5914b37a1b4aa32eee".decodeHex)
  f.add("b9300670b4c5366e95b2699e8b18bc75e5f729c5".decodeHex)
  assert(f.exists("99108ad8ed9bb6274d3980bab5a85c048f0950c8".decodeHex))
  assert(!f.exists("19108ad8ed9bb6274d3980bab5a85c048f0950c8".decodeHex))
  assert(f.exists("b5a2c786d9ef4658287ced5914b37a1b4aa32eee".decodeHex))
  assert(f.exists("b9300670b4c5366e95b2699e8b18bc75e5f729c5".decodeHex))
  assert(FilterLoadPayload(f).bytes.toArray.encodeHex == "03ce4299050000000100008002")
  TestBloomFilterBitJ3
}

object TestBloomFilterBitJ3 {
  println("Bloom filter last test")
  // https://github.com/bitcoinj/bitcoinj/blob/a7cad0ede447d4bcba7dd55639df442a408df6fb/core/src/test/java/org/bitcoinj/core/BloomFilterTest.java
  BitcoinS.isMainNet = true
  val f = BloomFilter.getFilter(5 /* nItems */, 0.001, 0 /* nTweak */, 2 /* nFlags = UPDATE_P2PUBKEY_ONLY */)
  val prv = PrvKey.getPrvKeyP2PKH("5Kg1gnAjaLfKiwhhPpGS3QfRg2m6awQvaj98JCZBZQ5SuS2F15C")
  assert(prv.pubKey.address == "17Wx1GQfyPTNWpQMHrTwRSMTCAonSiZx9e")  
  val pub = new PubKey_P2PKH(ECCPubKey("03cb219f69f1b49468bd563239a86667e74a06fcba69ac50a08a5cbc42a5808e99"), true /* mainNet */)
  f.addPubKey(pub)
  f.addPubKey(prv.pubKey)
  val tx = new TxParser("01000000010000000000000000000000000000000000000000000000000000000000000000ffffffff0d038754030114062f503253482fffffffff01c05e559500000000232103cb219f69f1b49468bd563239a86667e74a06fcba69ac50a08a5cbc42a5808e99ac00000000".decodeHex).getTx
  f.addTx(tx.txid, 0)
  assert(FilterLoadPayload(f).bytes.toArray.encodeHex == "082ae5edc8e51d4a03080000000000000002")  
}

