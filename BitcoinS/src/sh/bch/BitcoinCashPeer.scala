
package sh.bch

import sh.btc.BitcoinS._
import sh.net.Peer

object BitcoinCashPeer {
  /* https://gist.github.com/scalahub/09dedd9a30ee6b42a61adf481f5830eb#file-bitcoincash-seed-servers-L53 */
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
    "electrum-testnet-abc.criptolayer.net"
  )
  val peers = seeds.map(Peer.connectTo)
}
