package sh

import sh.net._
import sh.btc.BitcoinUtil._
import sh.btc._
import sh.bch.BitcoinCashSNode
import sh.btc.BitcoinS._
import sh.util.StringUtil._
import sh.net.DataStructures._
import sh.net.NetUtil._
import sh.util.BytesUtil._
import sh.ecc.Util._
import sh.util.HashUtil._

object TestPeer extends App {
  /*    from:  https://github.com/bitcoin/bitcoin/blob/master/src/chainparams.cpp
        vSeeds.emplace_back("seed.bitcoin.sipa.be", true); // Pieter Wuille, only supports x1, x5, x9, and xd
        vSeeds.emplace_back("dnsseed.bluematt.me", true); // Matt Corallo, only supports x9
        vSeeds.emplace_back("dnsseed.bitcoin.dashjr.org", false); // Luke Dashjr
        vSeeds.emplace_back("seed.bitcoinstats.com", true); // Christian Decker, supports x1 - xf
        vSeeds.emplace_back("seed.bitcoin.jonasschnelli.ch", true); // Jonas Schnelli, only supports x1, x5, x9, and xd
        vSeeds.emplace_back("seed.btc.petertodd.org", true); // Peter Todd, only supports x1, x5, x9, and xd    */
  // isMainNet = false // se to true for main net (default)
  // Peer.debug = true // prints lot more info
  BitcoinSNode.addOnTxHandler("myTxHandler", tx => println(s"[tx] $tx"))
  BitcoinSNode.addOnBlkHandler("myBlkHandler", blk => println(s"[blk] $blk"))
  BitcoinSNode.connectTo("localhost", false)
  BitcoinSNode.start(false) // connect to seed nodes
  /*  // Example below how to push tx
      val hex = "010000000138ee5b9c86d9d4fde197dbba82749bb00c8c71eb62847d18ad56491d6eb15296000000006a4730440220734765e05d315bb6b1fefdda28d959f79422d4aba1e9d8e38d61e4d0d415a0a102202e41a83c9bd71d2c817fb30b4a5229d3f7530946e01153f52e7580c26be7087d0121039f53e45f8f18b8ed294378bda342eff69b2053debf27fbede7d2d6bd84be6235ffffffff014062b0070000000017a914a9974100aeee974a20cda9a2f545704a0ab54fdc8700000000"
      val tx = new TxParser(hex.decodeHex).getTx
      println("txid "+tx.txid)
      Thread.sleep(10000) // sleep for 10 seconds for peers to connect
      peers.foreach(_ ! tx) // send tx to peer to broadcast */
  println("Started ...")
  Thread.sleep(10000) // wait for connnect 10 secs
  println("Asking for blk1 ...")
  val (blk1, time1) = timed(BitcoinSNode.getBlock("00000000000000000012560afe84f2bcc3df39fa42da68d1102490bbbd91af31"))
  println(s"blk1 received in $time1 millis")
  val (blk2, time2) = timed(BitcoinSNode.getBlock("000000000000000000182c8e114c28b558b983ccb00746a7b00adf84cd49a938"))
  println(s"blk2 received in $time2 millis")
  def timed[T](f: => T) = {
    val st = System.currentTimeMillis
    (f, System.currentTimeMillis - st)
  }
}
object TestUAHFPeer { //extends App {
  /*    from:  https://github.com/bitcoin/bitcoin/blob/master/src/chainparams.cpp
        vSeeds.emplace_back("seed.bitcoin.sipa.be", true); // Pieter Wuille, only supports x1, x5, x9, and xd
        vSeeds.emplace_back("dnsseed.bluematt.me", true); // Matt Corallo, only supports x9
        vSeeds.emplace_back("dnsseed.bitcoin.dashjr.org", false); // Luke Dashjr
        vSeeds.emplace_back("seed.bitcoinstats.com", true); // Christian Decker, supports x1 - xf
        vSeeds.emplace_back("seed.bitcoin.jonasschnelli.ch", true); // Jonas Schnelli, only supports x1, x5, x9, and xd
        vSeeds.emplace_back("seed.btc.petertodd.org", true); // Peter Todd, only supports x1, x5, x9, and xd    */
  // isMainNet = false
  Peer.debug = true
  BitcoinCashSNode.start(true)
}
