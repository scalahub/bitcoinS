
package sh.btc

import BitcoinUtil._
import sh.util.BytesUtil._

abstract class AbstractParser(bytes:Array[Byte]) {
  var currCtr:Int = 0 // stores currently parsed byte from bytes
  
  protected def getAndIncrCtr = {
    val c = currCtr
    incrCtr(1) 
    c
  }
  
  protected def numBytesRemaining = bytes.size - currCtr
  
  protected def incrCtr(int:Int) = currCtr += int 
  
  def getNextBytes(int:Int) = (1 to int).map(i => bytes(getAndIncrCtr)) 
  
  protected def getBytes(from:Int, to:Int) = (from to to).map(i => bytes(i))
  
  def getNext32Hash = getHexFromLittleEndian(getNextBytes(32).toArray)
  
  // unsigned
  def getNext4UInt = getUInt4LittleEndian(getNextBytes(4).toArray)
  
  // signed. NOTE: uses direct byte conversion to signed number
  def getNext4SInt = getSInt4LittleEndian(getNextBytes(4).toArray)
  
  def getCompactInt = { 
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
