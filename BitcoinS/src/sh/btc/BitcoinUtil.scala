package sh.btc

import sh.util.Base58Check
import BitcoinUtil._
import sh.btc.DataStructures._
import sh.ecc.Util._


object BitcoinUtil {
  var debug = false
  var isMainNet = true // set to false for testnet
  
  //http://www.soroushjp.com/2014/12/20/bitcoin-multisig-the-hard-way-understanding-raw-multisignature-bitcoin-transactions/
  val OP_Dup = 0x76.toByte
  val OP_Hash160 = 0xa9.toByte
  val OP_EqualVerify = 0x88.toByte
  val OP_Equal = 0x87.toByte
  val OP_CheckSig = 0xac.toByte
  
  // address prefixes  /* https://en.bitcoin.it/wiki/List_of_address_prefixes  */
  val P2PKH  = 0x00.toByte
  val P2SH   = 0x05.toByte
  val T_P2PKH = 0x6F.toByte   // testnet
  val T_P2SH = 0xC4.toByte    // testnet
  
  type Amount = BigInt  
  type Address = String

  private val decimalDigits = 8
  
  def insertDecimal(bigInt:BigInt) = { // inserts decimal to a BigInt and converts result to String
    val posStr = bigInt.abs.toString
    val len = posStr.length
    val str = if (len < (decimalDigits+1)) ("0."+"0"*(decimalDigits-len)+posStr) else posStr.substring(0, len-decimalDigits)+"."+posStr.substring(len-decimalDigits)
    if (bigInt.signum < 0) "-"+str else str
  }
  def removeDecimal(s:String) = (
    new BigDecimal(new java.math.BigDecimal(s)) * BigDecimal("1"+"0"*decimalDigits)
  ).toBigInt.toString
  
  def getScriptPubKeyFromAddress(address:Address) = {
    val decoded = Base58Check.decode(address)             
    val (prefix, pubKeyHash) = (decoded(0), decoded.drop(1))  // first byte is network version and address type
    if (pubKeyHash.size != 20) throw new Exception(s"Expected 20 bytes for pubKeyHash. Found ${pubKeyHash.size}")
    /*  From the Bitcoin Wiki you can get the hex codes for the four opcodes in this locking script 
        (OP_DUP is 0x76, OP_HASH160 is 0xa9, OP_EQUALVERIFY is 0x88, and OP_CHECKSIG is 0xac).       */
    prefix match {
      case P2PKH|T_P2PKH => Seq(OP_Dup, OP_Hash160, pubKeyHash.size.toByte) ++ pubKeyHash ++ Seq(OP_EqualVerify, OP_CheckSig)
      case P2SH|T_P2SH => Seq(OP_Hash160, pubKeyHash.size.toByte) ++ pubKeyHash ++ Seq(OP_Equal)
      case any => throw new Exception(s"Unknown address prefix 0x${any.toHexString}")
    }
  }
  
  def getUInt4LittleEndian(bytes:Array[Byte]) = { // returns integer from 4 bytes int 
    if (bytes.size != 4) throw new Exception("Expected 4 bytes in UINT32. Found "+bytes.size)
    BigInt(bytes.reverse.encodeHex, 16).toLong 
  }

  def getUInt8LittleEndian(bytes:Array[Byte]) = { // returns integer from 4 bytes int 
    if (bytes.size != 8) throw new Exception("Expected 8 bytes in UINT64. Found "+bytes.size)
    BigInt(bytes.reverse.encodeHex, 16)
  }
  
  def getHexFromLittleEndian(bytes:Array[Byte]) = bytes.reverse.encodeHex
  
  def getBase58FromBytes(addrBytes:Array[Byte]) = 
    Base58Check.encodePlain(addrBytes ++ dsha256(addrBytes).take(4))
  
  def hash160(bytes:Seq[Byte]) = ripeMD160(sha256Bytes2Bytes(bytes.toArray))
  
  def getHashed(bytes:Seq[Byte]) = dsha256(bytes.toArray).reverse.encodeHex.toLowerCase
  
  def getAddrFromOutScript(outScript:Array[Byte]) = {
    // if its a simple pay to address (OP_DUP OP_HASH160 OP_PUSHDATXX)
    // https://bitcoin.stackexchange.com/a/19108/2075
    // OP_DUP = 0x76
    // OP_HASH160 = 0xA9
    if (outScript.isEmpty) None 
    else {
      if (outScript(0) == 0x76.toByte && outScript(1) == 0xA9.toByte) {        
        val numBytes = outScript(2).toInt
        //val addrBytes = 0x00.toByte +: outScript.drop(3).take(numBytes)
        val addrBytes = (if (isMainNet) 0x00.toByte else 111.toByte) +: outScript.drop(3).take(numBytes)
        Some(getBase58FromBytes(addrBytes)) // need to decode output
      } else if (outScript(0) == 0xA9.toByte){
        // http://www.soroushjp.com/2014/12/20/bitcoin-multisig-the-hard-way-understanding-raw-multisignature-bitcoin-transactions/       
        val numBytes = outScript(1).toInt
        val addrBytes = (if (isMainNet) 0x05.toByte else 0xC4.toByte) +: outScript.drop(2).take(numBytes)
        Some(getBase58FromBytes(addrBytes)) // need to decode output
      } else None      
    }
  }  
  
  val max4VarInt = BigInt("FFFFFFFF", 16).toLong
  val max2VarInt = BigInt("FFFF", 16).toLong  
  val max1VarInt = BigInt("FD", 16).toLong  

  def getFixedIntBytes(int:BigInt, len:Int) = {
    val value = int.toBytes 
    val pad = len - value.size // pad remaining out of len bytes
    (new Array[Byte](pad) ++ value).reverse // reverse because Little Endian
  }
  def getVarIntBytes(l:Long) = {
    // https://en.bitcoin.it/wiki/Protocol_documentation#Variable_length_integer    
    if (l < 0) throw new Exception("VarInt must be >= 0")
    if (l > max4VarInt) { // 8 bytes
      Seq(0xFF.toByte)++BigInt(l).toBytes.reverse
    } else {
      if (l > max2VarInt) { // 4 bytes
        Seq(0xFE.toByte)++BigInt(l).toBytes.reverse
      } else {
        if (l >= max1VarInt) { //2 bytes
          Seq(0xFD.toByte)++BigInt(l).toBytes.reverse
        } else { // 0 bytes   
          Seq(l.toByte)
        }
      }
    }
    /*
Integer can be encoded depending on the represented value to save space. Variable length integers always precede an array/vector of a type of data that may vary in length. Longer numbers are encoded in little endian.
Value	Storage length	Format
< 0xFD	1	uint8_t
<= 0xFFFF	3	0xFD followed by the length as uint16_t
<= 0xFFFF FFFF	5	0xFE followed by the length as uint32_t
-	9	0xFF followed by the length as uint64_t
     */
  }
  
  val versionBytes = Seq[Byte](0x00, 0x00, 0x00, 0x01).reverse
  
  def createNonSegWitTxRaw(ins:Seq[In], outs:Seq[Out], lockTime:Long = 0) = {
    val inCtrBytes = getVarIntBytes(ins.size)
    val inBytes = ins.flatMap{in =>
      val prevTxHashBytes = in.txHash.grouped(2).toSeq.reverse.mkString.decodeHex
      val vOutBytes = getFixedIntBytes(in.vOut, 4)
      val scriptSig = in.optScriptSig.getOrElse(Nil)
      val scriptSigBytes = getVarIntBytes(scriptSig.size) 
      prevTxHashBytes ++ vOutBytes ++ scriptSigBytes ++ scriptSig ++ in.seqNumBytes
    }
    val outCtrBytes = getVarIntBytes(outs.size)
    val outBytes = outs.flatMap{out =>
      val valueBytes = getFixedIntBytes(out.value, 8)
      val lockingScriptBytes = out.optScriptPubKey match {
        case Some(scriptPubKey) =>           
          val scriptPubKeySizeBytes = getVarIntBytes(scriptPubKey.size)
          scriptPubKeySizeBytes ++ scriptPubKey
        case _ => ??? // should not happen
      }
      valueBytes ++ lockingScriptBytes
    }
    val lockTimeBytes = getFixedIntBytes(lockTime, 4) // should be Seq[Byte](0x00, 0x00, 0x00, 0x00)
    versionBytes ++ inCtrBytes ++ inBytes ++ outCtrBytes ++ outBytes ++ lockTimeBytes
  }.toArray
  
  def createSegWitTxRaw(ins:Seq[In], outs:Seq[Out]):Array[Byte] = createSegWitTxRaw(ins.map((_, Wit(Nil))), outs)
  
  def createSegWitTxRaw(insWits:Seq[(In, Wit)], outs:Seq[Out], lockTime:Long = 0) = {    // inputs also contains amount
    val (ins, wits) = insWits.unzip
    val inBytes = ins.flatMap{in =>
      val prevTxHashBytes = in.txHash.grouped(2).toSeq.reverse.mkString.decodeHex
      val vOutBytes = getFixedIntBytes(BigInt(in.vOut), 4)
      val scriptSig = in.optScriptSig.getOrElse(Nil)
      val scriptSigBytes = getVarIntBytes(scriptSig.size) 
      prevTxHashBytes ++ vOutBytes ++ scriptSigBytes ++ scriptSig ++ in.seqNumBytes
    }
    val outBytes = outs.flatMap{out =>
      val valueBytes = getFixedIntBytes(out.value, 8)
      val lockingScriptBytes = out.optScriptPubKey match {
        case Some(scriptPubKey) =>           
          val scriptPubKeySizeBytes = getVarIntBytes(scriptPubKey.size)
          scriptPubKeySizeBytes ++ scriptPubKey
        case _ => ??? // should not happen
      }
      valueBytes ++ lockingScriptBytes
    }
    val isSegWit = wits.exists(_.data.nonEmpty)
    val witBytes = if (isSegWit) wits.flatMap{wit =>
      getVarIntBytes(wit.data.size) ++ wit.data.flatMap{stackItem =>
        val stackItemSize = getVarIntBytes(stackItem.size)
        stackItemSize ++ stackItem
      }      
    } else Nil
    val inCtrBytes = getVarIntBytes(ins.size)
    val outCtrBytes = getVarIntBytes(outs.size)
    val flagMarkerBytes = if (isSegWit) Seq(0x00.toByte, 0x01.toByte) else Nil // 1st byte after version is 00 for a segwit tx
    val lockTimeBytes = getFixedIntBytes(lockTime, 4) // should be Seq[Byte](0x00, 0x00, 0x00, 0x00)
    versionBytes ++ flagMarkerBytes ++ inCtrBytes ++ inBytes ++ outCtrBytes ++ outBytes ++ witBytes ++ lockTimeBytes
  }.toArray
  
  def getMessageToSignBitcoinD(message:String) = 
    Seq(magicBytes.size.toByte) ++ magicBytes ++ Seq(message.size.toByte) ++ message.getBytes
  
}
