
package sh.btc

import BitcoinS._
import sh.net.Node

object BitcoinSNode extends Node{
  lazy val id = "Bitcoin"
  val seeds = if (isMainNet) Seq(
    "seed.bitcoin.sipa.be", 
    "dnsseed.bluematt.me",  
    "dnsseed.bitcoin.dashjr.org",  
    "seed.bitcoinstats.com",
    "seed.bitcoin.jonasschnelli.ch",  
    "seed.btc.petertodd.org"
  ) else Seq(
    "testnet-seed.bitcoin.jonasschnelli.ch", 
    "seed.tbtc.petertodd.org",
    "testnet-seed.bluematt.me",
    "testnet-seed.bitcoin.schildbach.de"
  )
}
