
package sh.btc

import sh.util.StringUtil._
import sh.net.Node

class BitcoinSNode(isMainNet:Boolean) extends Node{
  lazy val id = "Bitcoin"  
  lazy val version:Int = 70003  
  lazy val userAgent:String = "/BitcoinS:0.1/"     
  lazy val serviceBit:Int = 0
  lazy val magicBytes = if (isMainNet) "F9BEB4D9".decodeHex else "0B110907".decodeHex
  
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
