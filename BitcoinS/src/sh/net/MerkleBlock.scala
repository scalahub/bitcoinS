
package sh.net

import sh.btc.DataStructures.BlkHeader

/*
An example header in hex:

02000000 ........................... Block version: 2

b6ff0b1b1680a2862a30ca44d346d9e8
910d334beb48ca0c0000000000000000 ... Hash of previous block's header
9d10aa52ee949386ca9385695f04ede2
70dda20810decd12bc9b048aaab31471 ... Merkle root

24d95a54 ........................... Unix time: 1415239972
30c31b18 ........................... Target: 0x1bc330 * 256**(0x18-3)
fe9f0864 ........................... Nonce

val header = versionBytes ++ prevBlockHashBytes++merkleRootBytes ++ timeBytes ++ nBits ++ nonceBytes
 
 */

case class MerkleBlock(header:BlkHeader, txCount:Int, hashes:Seq[Seq[Byte]], flags:Seq[Byte]){
    
}

