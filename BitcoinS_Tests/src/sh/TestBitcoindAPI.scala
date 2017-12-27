
package sh.btc


import sh.btc.DataStructures._
//import sh.ecc.Util._
import sh.ecc._
import sh.util.CurlJsonData

object TestBitcoindAPI extends App {
  BitcoinUtil.isMainNet = false
  val b = new BitcoindAPI("user", "password", "http://localhost:8332")
  
  1 to 10000 foreach {i =>
    val a = new PrvKey(i * 1000, true).pubKey.getAddress
    b.importAddress(a) // rescan is always false
  }
  println("tx "+b.getTransaction("b46d059b6984485e32a9743d8da7125c01f75ce5b39366eb402599d41cd82291"))
  println("confs "+b.getConfirmations("b46d059b6984485e32a9743d8da7125c01f75ce5b39366eb402599d41cd82291"))
  println("block "+b.getBlock("6409a5bb8f5dd74495fc16b2c5aeab40b2cdc0d18a716595479bb6ff2b60bf6b"))
  println("version "+b.getSoftwareVersion)
  println("import address1 "+b.importAddress("2N8hwP1WmJrFF5QWABn38y63uYLhnJYJYTF"))
  println("import address2 "+b.importAddress("n4obYN9q8h4GCazSfD9j7gUUSWwDqrnkWm"))
  println("getAddresses")
  b.getBlock(b.getBestBlockHash).txs foreach println
  val hex = b.createRawTransaction(
    Array(
      In("2550fd204745361fd828042423bcde2be000926bd2d4db901749b85f1592e24f", 0), 
      In("b2b7217210316e680ecd5ebbe38b6debfc1e2486f6f9a1cca6bdfe34ff32b274", 1),
      In("4792d9c13c37dc08dc05953ebf5533480927b961bcca59421a8f863aedb1e719", 2)
    ), 
    Array(
      Out(Some("2N8hwP1WmJrFF5QWABn38y63uYLhnJYJYTF"), 100000),
      Out(Some("tmGL4Hjcc8J1rkikD7jQqRvQvrpKtq7GbJW"), 200000)
    )
  )
  println("hex "+hex)
}

object ParserTest extends App{  // tests the tx parser of BitcoindAPI (that parses response of getrawtransaction)
  import ParserTestData._
  val (segWitTx, _) = Parser.parseTxXML(segWitXML)
  val (segWitTxFromJSON, _) = Parser.parseTxXML(CurlJsonData.jsonStringToXML(segWitJSON))
  assert(segWitTx.txid == segWitTxFromJSON.txid) // also other test
  val (nonSegWitTx, _) = Parser.parseTxXML(CurlJsonData.jsonStringToXML(nonSegWitJSON))
  assert(nonSegWitTx.txid == "1ccc8eb9f20925639cc0e4276ce1493e223cc94aeaebe7b1a66746a85e4ba125")
  assert(nonSegWitTx.segWitTxHash == "1ccc8eb9f20925639cc0e4276ce1493e223cc94aeaebe7b1a66746a85e4ba125")
  assert(segWitTx.segWitTxHash == "23c0084a8bfc410e46a1ba315c8cd393db45f8e80df7c3aabe0c175b2d6372f8")
  val wit = segWitTx.witnesses(0)
  import sh.ecc.Util._
  assert(wit.data(0).toArray.encodeHex.toLowerCase == "3044022055863405aab0bab1dc76e6ef2f573e40df6cfdf2f0ee8235c4b82fff61ba7514022078bc22c5c2954d7f2d2f6ff438e7c2e88bcf815d09067982e6340750decbf0d601")
  assert(wit.data(1).toArray.encodeHex.toLowerCase == "039f53e45f8f18b8ed294378bda342eff69b2053debf27fbede7d2d6bd84be6235")

}

object ParserTestData {
  
  val nonSegWitJSON = """{
  "txid": "1ccc8eb9f20925639cc0e4276ce1493e223cc94aeaebe7b1a66746a85e4ba125",
  "hash": "1ccc8eb9f20925639cc0e4276ce1493e223cc94aeaebe7b1a66746a85e4ba125",
  "version": 1,
  "size": 370,
  "vsize": 370,
  "locktime": 0,
  "vin": [
    {
      "txid": "f6b194b41cac5c7537797f2085323b52672c0ea5583a6ce6794c5f8c59b872e2",
      "vout": 0,
      "scriptSig": {
        "asm": "3044022045f3c2fc18daee6e8a0f9885e7c44005b99b84d5704838216ec398ca291b81820220485eaf93fbd58a39f81f49cff36bc6f0e8c5d04dff55bbb180806a4884b1d84f[ALL] 0352cd03c24fd51b84a9c17c64db26f34eea7e31c3cc0e4b6ccb23a9666b3a29bb",
        "hex": "473044022045f3c2fc18daee6e8a0f9885e7c44005b99b84d5704838216ec398ca291b81820220485eaf93fbd58a39f81f49cff36bc6f0e8c5d04dff55bbb180806a4884b1d84f01210352cd03c24fd51b84a9c17c64db26f34eea7e31c3cc0e4b6ccb23a9666b3a29bb"
      },
      "sequence": 4294967295
    }, 
    {
      "txid": "b426d0285d79b345b116ddee65b0e486d045161aea3a07c00bbc0082185d1673",
      "vout": 0,
      "scriptSig": {
        "asm": "304402200e00c0b1627259946fbe9de7ce45f3f1f3ad472b6351e86bda5adb24db069de602202057f002ec4a97e135824c937ee2fb32cd2e147cab67b248da985af2383c68b2[ALL] 02563e1263a75869805449cb59e5da2e8a2613c05a2cff37c78e2c626fcd0cb98b",
        "hex": "47304402200e00c0b1627259946fbe9de7ce45f3f1f3ad472b6351e86bda5adb24db069de602202057f002ec4a97e135824c937ee2fb32cd2e147cab67b248da985af2383c68b2012102563e1263a75869805449cb59e5da2e8a2613c05a2cff37c78e2c626fcd0cb98b"
      },
      "sequence": 4294967295
    }
  ],
  "vout": [
    {
      "value": 0.00095030,
      "n": 0,
      "scriptPubKey": {
        "asm": "OP_DUP OP_HASH160 726b76968926bc5e1010ba377ab7ba1341083ce3 OP_EQUALVERIFY OP_CHECKSIG",
        "hex": "76a914726b76968926bc5e1010ba377ab7ba1341083ce388ac",
        "reqSigs": 1,
        "type": "pubkeyhash",
        "addresses": [
          "mqwx4bhhNegZg1kfAYqpvXibVAvkBWzsdd"
        ]
      }
    }, 
    {
      "value": 0.01100000,
      "n": 1,
      "scriptPubKey": {
        "asm": "OP_HASH160 581917602a2adeb2bc606ea4ef87354248f65061 OP_EQUAL",
        "hex": "a914581917602a2adeb2bc606ea4ef87354248f6506187",
        "reqSigs": 1,
        "type": "scripthash",
        "addresses": [
          "2N1H3WYGSMpYjibLQNvUZJHupfCpFBw5RXo"
        ]
      }
    }
  ],
  "hex": "0100000002e272b8598c5f4c79e66c3a58a50e2c67523b3285207f7937755cac1cb494b1f6000000006a473044022045f3c2fc18daee6e8a0f9885e7c44005b99b84d5704838216ec398ca291b81820220485eaf93fbd58a39f81f49cff36bc6f0e8c5d04dff55bbb180806a4884b1d84f01210352cd03c24fd51b84a9c17c64db26f34eea7e31c3cc0e4b6ccb23a9666b3a29bbffffffff73165d188200bc0bc0073aea1a1645d086e4b065eedd16b145b3795d28d026b4000000006a47304402200e00c0b1627259946fbe9de7ce45f3f1f3ad472b6351e86bda5adb24db069de602202057f002ec4a97e135824c937ee2fb32cd2e147cab67b248da985af2383c68b2012102563e1263a75869805449cb59e5da2e8a2613c05a2cff37c78e2c626fcd0cb98bffffffff0236730100000000001976a914726b76968926bc5e1010ba377ab7ba1341083ce388ace0c810000000000017a914581917602a2adeb2bc606ea4ef87354248f650618700000000",
  "blockhash": "00000000000611048b03364398d9d8a08f2b3a08d8b0e81f3ff7f5b737bc0553",
  "confirmations": 81,
  "time": 1514075920,
  "blocktime": 1514075920
}
"""
  val segWitJSON = """{
  "txid": "43993aa1ed51f6a1b9229fae9cd47aaa46d78f71819e30e4b9d8a1e7d649109a",
  "hash": "23c0084a8bfc410e46a1ba315c8cd393db45f8e80df7c3aabe0c175b2d6372f8",
  "version": 1,
  "size": 215,
  "vsize": 134,
  "locktime": 0,
  "vin": [
    {
      "txid": "b49f3d6d15f2bdd9217ba3caaf1bb1f2d9875c9657e6b0ac7a0ef841d486ad1d",
      "vout": 1,
      "scriptSig": {
        "asm": "001497acce1f07fc7d118f4a0cea16b72d567f151cb6",
        "hex": "16001497acce1f07fc7d118f4a0cea16b72d567f151cb6"
      },
      "txinwitness": [
        "3044022055863405aab0bab1dc76e6ef2f573e40df6cfdf2f0ee8235c4b82fff61ba7514022078bc22c5c2954d7f2d2f6ff438e7c2e88bcf815d09067982e6340750decbf0d601", 
        "039f53e45f8f18b8ed294378bda342eff69b2053debf27fbede7d2d6bd84be6235"
      ],
      "sequence": 4294967295
    }
  ],
  "vout": [
    {
      "value": 0.19900000,
      "n": 0,
      "scriptPubKey": {
        "asm": "OP_HASH160 a9974100aeee974a20cda9a2f545704a0ab54fdc OP_EQUAL",
        "hex": "a914a9974100aeee974a20cda9a2f545704a0ab54fdc87",
        "reqSigs": 1,
        "type": "scripthash",
        "addresses": [
          "2N8hwP1WmJrFF5QWABn38y63uYLhnJYJYTF"
        ]
      }
    }
  ],
  "hex": "010000000001011dad86d441f80e7aacb0e657965c87d9f2b11bafcaa37b21d9bdf2156d3d9fb4010000001716001497acce1f07fc7d118f4a0cea16b72d567f151cb6ffffffff0160a62f010000000017a914a9974100aeee974a20cda9a2f545704a0ab54fdc8702473044022055863405aab0bab1dc76e6ef2f573e40df6cfdf2f0ee8235c4b82fff61ba7514022078bc22c5c2954d7f2d2f6ff438e7c2e88bcf815d09067982e6340750decbf0d60121039f53e45f8f18b8ed294378bda342eff69b2053debf27fbede7d2d6bd84be623500000000",
  "blockhash": "00000000000003aaf98743c998b37e257acad87f1eeb4888149a7f6e0564ac46",
  "confirmations": 84,
  "time": 1514072245,
  "blocktime": 1514072245
}
"""
  val segWitXML = 
    <JSON><vsize>134</vsize><locktime>0</locktime><txid>43993aa1ed51f6a1b9229fae9cd47aaa46d78f71819e30e4b9d8a1e7d649109a</txid>
    <confirmations>84</confirmations><version>1</version>
    <vout>
      <scriptPubKey><addresses>2N8hwP1WmJrFF5QWABn38y63uYLhnJYJYTF</addresses>
      <asm>OP_HASH160 a9974100aeee974a20cda9a2f545704a0ab54fdc OP_EQUAL</asm>
      <hex>a914a9974100aeee974a20cda9a2f545704a0ab54fdc87</hex>
        <type>scripthash</type>
        <reqSigs>1</reqSigs>
      </scriptPubKey><value>0.199</value><n>0</n>
    </vout>
    <blockhash>00000000000003aaf98743c998b37e257acad87f1eeb4888149a7f6e0564ac46</blockhash><size>215</size><blocktime>1514072245</blocktime>
    <vin>
      <sequence>4294967295</sequence>
      <scriptSig>
        <asm>001497acce1f07fc7d118f4a0cea16b72d567f151cb6</asm>
        <hex>16001497acce1f07fc7d118f4a0cea16b72d567f151cb6</hex>
      </scriptSig>
      <txid>b49f3d6d15f2bdd9217ba3caaf1bb1f2d9875c9657e6b0ac7a0ef841d486ad1d</txid>
      <txinwitness>3044022055863405aab0bab1dc76e6ef2f573e40df6cfdf2f0ee8235c4b82fff61ba7514022078bc22c5c2954d7f2d2f6ff438e7c2e88bcf815d09067982e6340750decbf0d601</txinwitness>
      <txinwitness>039f53e45f8f18b8ed294378bda342eff69b2053debf27fbede7d2d6bd84be6235</txinwitness>
      <vout>1</vout>
    </vin>
    <hex>010000000001011dad86d441f80e7aacb0e657965c87d9f2b11bafcaa37b21d9bdf2156d3d9fb4010000001716001497acce1f07fc7d118f4a0cea16b72d567f151cb6ffffffff0160a62f010000000017a914a9974100aeee974a20cda9a2f545704a0ab54fdc8702473044022055863405aab0bab1dc76e6ef2f573e40df6cfdf2f0ee8235c4b82fff61ba7514022078bc22c5c2954d7f2d2f6ff438e7c2e88bcf815d09067982e6340750decbf0d60121039f53e45f8f18b8ed294378bda342eff69b2053debf27fbede7d2d6bd84be623500000000</hex><time>1514072245</time><hash>23c0084a8bfc410e46a1ba315c8cd393db45f8e80df7c3aabe0c175b2d6372f8</hash>
    </JSON>   

}