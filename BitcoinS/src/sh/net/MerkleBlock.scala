
package sh.net

import sh.btc.DataStructures.BlkHeader
import sh.net.NetUtil.Char32
import sh.util.BytesUtil._
import sh.util.HashUtil._
import sh.util.StringUtil._

private sealed trait MerkleNode

private case class Intermediate(leftChild:MerkleNode, optRightChild:Option[MerkleNode]) extends MerkleNode {
  def this(i:Seq[MerkleNode]) = this(i(0), if (i.size > 1) Some(i(1)) else None)
}

private case class Leaf(num:Int) extends MerkleNode

object MerkleBlock {
  // Returns hash (say h) of node along with list of matched filter hashes while computing h
  private def getHashes(node:MerkleNode, flagBits:Iterator[Boolean], remainingHashes:Iterator[Array[Byte]]):(Array[Byte], Seq[Array[Byte]]) = {
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
    // flag bits, packed per 8 in a byte, least significant bit first (including standard varint size prefix)
    val flagIterator = flags.getBitsLsbFirst.toIterator    
    val hashesIterator = hashes.toIterator
    val (rootHash, matched) = getHashes(currNodes(0), flagIterator, hashesIterator) // currNodes(0) is root    
    val remaingFlags = flagIterator.toArray    
    val valid = header.merkleRoot == Char32(rootHash).rpcHash && hashesIterator.isEmpty &&
                (flagIterator.isEmpty || (remaingFlags.size < 8 && remaingFlags.forall(!_)))
    
    (valid, if (valid) matched else Nil)
  } catch {
    case a:Any =>
      a.printStackTrace
      (false, Nil)
  }
  
}

