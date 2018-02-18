
package sh.bch

import sh.btc.BitcoinS
import sh.btc.BitcoinS._
import sh.net.NetUtil
import sh.net.Node
import sh.util.StringUtil._


object BitcoinCashSNode extends Node {
  /* https://gist.github.com/scalahub/09dedd9a30ee6b42a61adf481f5830eb#file-bitcoincash-seed-servers-L53 */
  lazy val id = "BitcoinCash"
  BitcoinS.defaultUserAgent = "/BitcoinCashS 1.0/"
  BitcoinS.ourServiceBit = 0x20
  NetUtil.magicBytes = "e3e1f3e8".decodeHex
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
