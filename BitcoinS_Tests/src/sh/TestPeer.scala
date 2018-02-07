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
  // isMainNet = false // set to true for main net (default)
  Peer.debug = true // prints a lot of info
   
  // Below shows how to add handlers for events (block or tx received)
  BitcoinSNode.addOnTxHandler("myTxHandler", tx => println(s"[tx] $tx"))
  BitcoinSNode.addOnBlkHandler("myBlkHandler", blk => println(s"[blk] $blk"))
  
  BitcoinSNode.connectTo("localhost", false) // connect to given node (false implies disable tx relay)
  BitcoinSNode.connectToAllSeeds(false) // connect to seed nodes (false implies disable tx relay)
  
  Thread.sleep(10000) // wait for connnect 10 secs
  
  println("Started ...")
  
  /*  // Example below how to push tx
      val hex = "010000000138ee5b9c86d9d4fde197dbba82749bb00c8c71eb62847d18ad56491d6eb15296000000006a4730440220734765e05d315bb6b1fefdda28d959f79422d4aba1e9d8e38d61e4d0d415a0a102202e41a83c9bd71d2c817fb30b4a5229d3f7530946e01153f52e7580c26be7087d0121039f53e45f8f18b8ed294378bda342eff69b2053debf27fbede7d2d6bd84be6235ffffffff014062b0070000000017a914a9974100aeee974a20cda9a2f545704a0ab54fdc8700000000"
      val tx = new TxParser(hex.decodeHex).getTx
      BitcoinSNode.pushTx(tx) // send tx to PeerGroup to broadcast */
     
  println("Asking for blk1 ...")
  val (blk1, time1) = timed(BitcoinSNode.getBlock("00000000000000000012560afe84f2bcc3df39fa42da68d1102490bbbd91af31"))
  println(s"blk1 received in $time1 millis")
  val (blk2, time2) = timed(BitcoinSNode.getBlock("000000000000000000182c8e114c28b558b983ccb00746a7b00adf84cd49a938"))
  println(s"blk2 received in $time2 millis")
  
  def timed[T](f: => T) = { // takes a method f outputting T and times it (i.e., finds how many millis did it take to invoke f)
    val st = System.currentTimeMillis
    (f, System.currentTimeMillis - st)
  }
}
object TestUAHFPeer extends App {
  isMainNet = false // testnet has some issues.. to resolve.
  Peer.debug = true
  BitcoinCashSNode.connectToAllSeeds(true)
  Thread.sleep(10000)
  val hex = "01000000023dea0ef9fb5e44740fed3f796a0a8560f0ef23cde48509da42d1c1e21093dfaa000000008a4730440220255cb99927119b6fa35529cad83ae37346f99aedd46be63ac440ddd74599317d022075b71dfcc3ecb33e66a654be471f1e208cfe991d75ab6f1669861081bb3ef7c14141046d163a922667b1e312fd9ef1623fa717ff1e05711c2b704cdc545128ea606b1b1d2234787277f170782a0e6b129acf3d27f222f39bc1b362f661f7a6de2f61d2ffffffff0bb44012b22318fe416ce34ff4cb7e9ba669a5fb2b538022f98c2d759e111672020000008b483045022100c1433ac28e39a9836688a78d04add2e081417513c43579bc664a4502070b34db02200ca8133867ecc972d61391d024a1b6afc7c29be8e5da9639b70d2ba94c57efd54141046d163a922667b1e312fd9ef1623fa717ff1e05711c2b704cdc545128ea606b1b1d2234787277f170782a0e6b129acf3d27f222f39bc1b362f661f7a6de2f61d2ffffffff02b8a2754d000000001976a91409fed3e08e624b23dbbacc77f7b2a39998351a6888ac20a10700000000001976a914fc0a8abe52055d55223d9e5d387f87ca7c320c9c88ac00000000"
  val tx = new TxParser(hex.decodeHex).getTx
  BitcoinCashSNode.pushTx(tx) // send tx to PeerGroup to broadcast */
  println("Tx pushed")
}
