
package sh.btc

import BitcoinUtil._

abstract class AbstractParser(bytes:Array[Byte]) {
  protected var currCtr:Int = 0 // stores currently parsed byte from bytes
  
  protected def getAndIncrCtr = {
    val c = currCtr
    incrCtr(1) 
    c
  }
  protected def incrCtr(int:Int) = currCtr += int 
  
  protected def getNextBytes(int:Int) = (1 to int).map(i => bytes(getAndIncrCtr)) 
  
  protected def getBytes(from:Int, to:Int) = (from to to).map(i => bytes(i))
  
  protected def getNext32Hash = getHashFromBytes(getNextBytes(32).toArray)
  
  protected def getNext4Int = getUInt4LittleEndian(getNextBytes(4).toArray)
  
  protected def getCompactInt = { 
    val ctr = getAndIncrCtr
    val nextNumBytes = bytes(ctr) & 0xFF match {
      case 0xff => 8 // next four bytes encode size
      case 0xfe => 4 // next three bytes encode size
      case 0xfd => 2 // next two bytes encode size
      case any  => 0 // only this byte encodes size
    }
    if (nextNumBytes == 0) bytes(ctr) & 0xFF
    else BigInt(getNextBytes(nextNumBytes).toArray.reverse).toInt
  }
  // Takes any function f outputting T. Returns the output of f along with the number of bytes consumed in computing f
  protected def usingBytes[T](f: => T):(T, Seq[Byte]) = {
    val st = currCtr
    val t = f
    (t, getBytes(st, currCtr - 1))
  }
  
}