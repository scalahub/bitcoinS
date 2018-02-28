
package sh.bch

import sh.net.Node
import sh.util.StringUtil._

class BitcoinCashSNode(isMainNet:Boolean) extends Node {
  lazy val id = "BitcoinCash"
  
  lazy val version:Int = 70003  
  lazy val userAgent:String = "/BitcoinCashS 1.0/"    
  lazy val serviceBit:Int = 0x20
  lazy val magicBytes = if (isMainNet) "e3e1f3e8".decodeHex else "0b110907".decodeHex

  /* e3e1f3e8 found via wireshark
     0b110907 found via https://github.com/ayeowch/bitnodes/blob/e298741dec9809f55585a83c64600f06edc9d233/start.sh#L31 
     seeds from https://gist.github.com/scalahub/09dedd9a30ee6b42a61adf481f5830eb#file-bitcoincash-seed-servers-L53 */
  lazy val seeds = if (isMainNet) Seq(
    "abc.vom-stausee.de",
    "abc1.hsmiths.com", 
    "bch.arihanc.com", 
    "35.157.238.5",
    "electrumx-cash.1209k.com", 
    "shsmithgoggryfbx.onion", 
    "mash.1209k.com", 
    "electroncash.bitcoinplug.com", 
    "bch.curalle.ovh",
    "electron.ueo.ch",
    "electron-cash.dragon.zone",
    "electroncash.cascharia.com"
  ) else Seq(
    "180.235.49.196",
    "electrum-testnet-abc.criptolayer.net",
    "testnet-seed-abc.bitcoinforks.org"
  )
}
