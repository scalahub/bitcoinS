
package sh.net

import sh.btc.DataStructures.BlkHeader
import sh.net.NetUtil.Char32
import sh.util.BytesUtil._
import sh.util.HashUtil._
import sh.util.StringUtil._

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
trait MerkleNode {
  //var optHash:Option[Array[Byte]] = None
}
case class Intermediate(leftClild:MerkleNode, optRightChild:Option[MerkleNode]) extends MerkleNode {
  def this(i:Seq[MerkleNode]) = this(i(0), if (i.size > 1) Some(i(1)) else None)
}
case class Leaf(num:Int) extends MerkleNode { // num is from 1 to numTx
  //var matched:Boolean = false
}
object MerkleBlock {
  def getHashes(node:MerkleNode, flagBits:Iterator[Boolean], remainingHashes:Iterator[Array[Byte]]):(Array[Byte], Seq[Array[Byte]]) = {
    if (flagBits.next){
      node match{
        case Intermediate(leftChild, optRightChild) =>
          val (lHash, lMatched) = getHashes(leftChild, flagBits, remainingHashes)
          val (rHash, rMatched) = optRightChild.map{rightChild =>
            val (r, rM) = getHashes(rightChild, flagBits, remainingHashes)
            if ((lHash zip r) forall {case (x, y) => x == y}) throw new Exception("L and R hashes must be different")
            (r, rM)
          }.getOrElse((lHash, Nil))
          (dsha256(lHash ++ rHash), lMatched ++ rMatched)          
        case l:Leaf =>
          val hash = remainingHashes.next
          (hash, Seq(hash))
      }
    } else {
      val hash = remainingHashes.next
      (hash, Nil)
    }
  }
  
}
case class MerkleBlock(header:BlkHeader, txCount:Int, hashes:Seq[Array[Byte]], flags:Array[Byte]){
  import MerkleBlock._
  val (isValid, matchedHashes) = try {
    var currNodes:Seq[MerkleNode] = (1 to txCount).map(Leaf)
    while (currNodes.size > 1) {
      currNodes = currNodes.grouped(2).map(new Intermediate(_)).toSeq
    }
    val root = currNodes(0)

    // flag bits, packed per 8 in a byte, least significant bit first (including standard varint size prefix)
    val flagIterator = flags.getBitsLsbFirst.toIterator
    
    val hashesIterator = hashes.toIterator
    
    val (rootHash, matched) = getHashes(root, flagIterator, hashesIterator)
    
    val remaingFlags = flagIterator.toArray
    
    val valid = header.merkleRoot == Char32(rootHash).rpcHash &&
    hashesIterator.isEmpty &&
    (flagIterator.isEmpty || (remaingFlags.size < 8 && remaingFlags.forall(!_)))
    
    (valid, if (valid) matched else Nil)
  } catch {
    case a:Any =>
      a.printStackTrace
      (false, Nil)
  }
  
}

