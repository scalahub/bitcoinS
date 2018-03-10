
package sh.bch

import sh.net.Node
import sh.util.StringUtil._

class BitcoinCashSNode(isMainNet:Boolean) extends Node {
  lazy val id = "BitcoinCash"
  
  lazy val version:Int = 70003  
  lazy val userAgent:String = "/BitcoinCashS 1.0/"    

  /* Notes about constants:
     0x20 from specs
     e3e1f3e8 found via wireshark
     f4e5f3f4 found via https://lists.linuxfoundation.org/pipermail/bitcoin-ml/2017-August/000072.html
     
   Magic:
    mainnet: 0xe3, 0xe1, 0xf3, 0xe8
    testnet: 0xf4, 0xe5, 0xf3, 0xf4
    regtest: 0xda, 0xb5, 0xbf, 0xfa

     seeds from https://gist.github.com/scalahub/09dedd9a30ee6b42a61adf481f5830eb#file-bitcoincash-seed-servers-L53 */
  
  lazy val serviceBit:Int = 0x20
  lazy val magicBytes = if (isMainNet) "e3e1f3e8".decodeHex else "f4e5f3f4".decodeHex

  val seeds = if (isMainNet) Seq(
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
