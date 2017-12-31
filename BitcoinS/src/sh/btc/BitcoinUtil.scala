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
  val OP_Return = 0x6a.toByte
  
  val OP_PushData = 0x4b.toByte
  
  val OP_PushData1 = 0x4c.toByte
  val OP_PushData2 = 0x4d.toByte
  val OP_PushData4 = 0x4e.toByte
  
  // address prefixes start /* https://en.bitcoin.it/wiki/List_of_address_prefixes  */
  val P2PKH  = 0x00.toByte
  val P2SH   = 0x05.toByte
  
  val T_P2PKH = 0x6f.toByte   // testnet
  val T_P2SH = 0xc4.toByte    // testnet
  
  //  val DATA = 0xc8.toByte // = 200 decimal. Custom (created for storing data. Not a standard!)
  //  val T_DATA = 0xcd.toByte // = 205 decimal. Custom (created for storing data. Not a standard!) TESTNET
  // NOTE: Do we need separate TESTNET code for DATA (above) and UNKNOWN (below) ?
  
  val UNKNOWN = 0x29.toByte // = 41 decimal. Results in starting with r. Custom (created for denoting any other unknown script types. Not a standard!)
  val T_UNKNOWN = 0xff.toByte // = 255 decimal. Results in starting with 6. Custom (created for denoting any other unknown script types. Not a standard!) TESTNET  
  // address prefixes end
  
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
  
  def getScriptPubKeyFromAddress(address:Address):Seq[Byte] = {
    val decoded = Base58Check.decode(address)             
    val (prefix, pubKeyHashOrData) = (decoded(0), decoded.drop(1))  // first byte is network version and address type
    /*  From the Bitcoin Wiki you can get the hex codes for the four opcodes in this locking script 
        (OP_DUP is 0x76, OP_HASH160 is 0xa9, OP_EQUALVERIFY is 0x88, and OP_CHECKSIG is 0xac).       */
    prefix match {
      case P2PKH|T_P2PKH => 
        if (pubKeyHashOrData.size != 20) throw new Exception(s"Expected 20 bytes for pubKeyHashOrData. Found ${pubKeyHashOrData.size}")
        Seq(OP_Dup, OP_Hash160, pubKeyHashOrData.size.toByte) ++ pubKeyHashOrData ++ Seq(OP_EqualVerify, OP_CheckSig)
      case P2SH|T_P2SH => 
        if (pubKeyHashOrData.size != 20) throw new Exception(s"Expected 20 bytes for pubKeyHashOrData. Found ${pubKeyHashOrData.size}")
        Seq(OP_Hash160, pubKeyHashOrData.size.toByte) ++ pubKeyHashOrData ++ Seq(OP_Equal)
      //      case DATA|T_DATA => // avoid DATA case and let UNKNOWN handle it
      //        val dataScript = pubKeyHashOrData // data includes push instruction, not just data (i.e., entire script)
      //        if (dataScript(0) > OP_PushData4) throw new Exception(s"Expected OP_PushData instruction at byte 0 of dataScript")
      //        Seq(OP_Return) ++ dataScript
      case UNKNOWN|T_UNKNOWN => // unknown script (non standard)
        pubKeyHashOrData 
      case any => throw new Exception(s"Unknown address prefix 0x${any.toHexString}")
    }
  }
  
  // UNSIGNED
  def getUInt4LittleEndian(bytes:Array[Byte]) = { // returns integer from 4 bytes int 
    if (bytes.size != 4) throw new Exception("Expected 4 bytes in UINT32. Found "+bytes.size)
    BigInt(bytes.reverse.encodeHex, 16).toLong 
  }
  
  // SIGNED
  def getSInt4LittleEndian(bytes:Array[Byte]) = { // returns signed integer from 4 bytes int 
    if (bytes.size != 4) throw new Exception("Expected 4 bytes in UINT32. Found "+bytes.size)
    BigInt(bytes.reverse).toLong 
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
    // OP_RETURN = 0x6A
    // http://www.soroushjp.com/2014/12/20/bitcoin-multisig-the-hard-way-understanding-raw-multisignature-bitcoin-transactions/       
    if (outScript.isEmpty) None 
    else {
      if (outScript(0) == OP_Dup && outScript(1) == OP_Hash160) {       
        // P2PKH standard output
        val numBytes = outScript(2).toInt
        val addrBytes = (if (isMainNet) P2PKH else T_P2PKH) +: outScript.drop(3).take(numBytes)
        Some(getBase58FromBytes(addrBytes)) // need to decode output
      } else if (outScript(0) == OP_Hash160){
        // P2SH standard output
        val numBytes = outScript(1).toInt
        val addrBytes = (if (isMainNet) P2SH else T_P2SH) +: outScript.drop(2).take(numBytes)
        Some(getBase58FromBytes(addrBytes)) // need to decode output
      //      } else if (outScript(0) == OP_Return) { 
      //        // Data (non-standard) output
      //        // data read from next byte
      //        // See if we can ignore Data type (i.e., OP_Return) and handle it as unknwon (next case after else)
      //        val (dataSizeBytes, dataSizeNumBytes) = outScript(1) match {
      //          case i if i <= OP_PushData => (outScript.drop(1).take(1), 0) // this byte contains numner of items
      //          case OP_PushData1 =>  (outScript.drop(2).take(1),         1) // next byte contains number of items to push
      //          case OP_PushData2 =>  (outScript.drop(2).take(2).reverse, 2) // next 2 bytes contains number of items to push 
      //          case OP_PushData4 =>  (outScript.drop(2).take(4).reverse, 4) // next 4 bytes contains number of items to push
      //          case _ => ??? // should not happen as per wiki: https://en.bitcoin.it/wiki/Script
      //        }        
      //        val dataSize = BigInt(dataSizeBytes).toInt // check issue with negatives !!!
      //        val data = outScript.drop(2+dataSizeNumBytes).take(dataSize)
      //        
      //        // our own custom address type! (can be used to create a tx that writes data to blockchain)
      //        // add OP_Push instructions to data bytes as well. We don't care
      //        val dataScript = outScript.drop(1).take(1 + dataSizeNumBytes) // script pushing data
      //        val dataBytes = (if (isMainNet) DATA else T_DATA) +: (dataScript ++ data)
      //        Some(getBase58FromBytes(dataBytes))
      } else {
        // Unknown type (non-standard) output. 
        // example: https://blockchain.info/tx/9a223b09449cdd383206db29fe7f5ea31e0154d9fb30eabafe36eb8a9a33d22f?format=hex
        // (txid 9a223b09449cdd383206db29fe7f5ea31e0154d9fb30eabafe36eb8a9a33d22f)
        // uses the script 52534b424c4f434b3addbf517adf8ffd4bca7751505b39c9013a0d1fd479fc4e901b39dd57b347c624 (hex)
        // which uses the opcodes 0x52 (push "2" to stack) and 0x52 (push "3" to stack) 
        // See https://en.bitcoin.it/wiki/Script#Constants 
        val unknownScriptBytes = (if (isMainNet) UNKNOWN else T_UNKNOWN) +: outScript
        Some(getBase58FromBytes(unknownScriptBytes))
      }
    }
  }  
  
  val max4VarInt = BigInt("FFFFFFFF", 16).toLong
  val max2VarInt = BigInt("FFFF", 16).toLong  
  val max1VarInt = BigInt("FD", 16).toLong  

  def getFixedIntBytes(bigInt:BigInt, len:Int) = {
    val value = bigInt.toBytes // converts using Hex (toBytes is a custom method added via implicit def) 
    val padLen = len - value.size // pad remaining out of len bytes
    val pad = Array.fill(padLen)(if (bigInt < 0) 0xFF.toByte else 0x00.toByte) // negatives padded with 0xFF
    (pad ++ value).reverse // reverse because Little Endian
  }

  def getVarIntBytes(l:Long) = {
    // https://en.bitcoin.it/wiki/Protocol_documentation#Variable_length_integer    
    if (l < 0) throw new Exception("VarInt must be >= 0")
    if (l > max4VarInt) { // 8 bytes      
      Seq(0xFF.toByte)++getFixedIntBytes(l, 8) 
    } else {
      if (l > max2VarInt) { // 4 bytes
        Seq(0xFE.toByte)++getFixedIntBytes(l, 4)
      } else {
        if (l >= max1VarInt) { //2 bytes
          Seq(0xFD.toByte)++getFixedIntBytes(l, 2)
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
  
  //val versionBytes = Seq[Byte](0x00, 0x00, 0x00, 0x01).reverse
  
  def toggleEndianString(hex:String) = {
    // little to big and vice versa on each call
    if (hex.size % 2 == 1) throw new Exception(s"Hex has odd number of chars (${hex.size})")
    hex.grouped(2).toSeq.reverse.mkString.decodeHex
  }
  def createNonSegWitTxRaw(ins:Seq[TxIn], outs:Seq[TxOut]) = createNonSegWitTxRawAdvanced(1, ins, outs, 0)
  
  def createNonSegWitTxRawAdvanced(version:Long, ins:Seq[TxIn], outs:Seq[TxOut], lockTime:Long) = {
    val inCtrBytes = getVarIntBytes(ins.size)
    val inBytes = ins.flatMap{in =>
      val prevTxHashBytes = toggleEndianString(in.txHash)
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
    val versionBytes = getFixedIntBytes(version, 4)
    val lockTimeBytes = getFixedIntBytes(lockTime, 4) // should be Seq[Byte](0x00, 0x00, 0x00, 0x00)
    versionBytes ++ inCtrBytes ++ inBytes ++ outCtrBytes ++ outBytes ++ lockTimeBytes
  }.toArray
  
  def createSegWitTxRaw(ins:Seq[TxIn], outs:Seq[TxOut]):Array[Byte] = createSegWitTxRawAdvanced(1, ins.map((_, TxWit(Nil))), outs, 0)
  
  //def createSegWitTxRaw(insWits:Seq[(In, Wit)], outs:Seq[Out]) = createSegWitTxRawAdvanced(1, insWits:Seq[(In, Wit)], outs:Seq[Out], 0)
  
  def createSegWitTxRawAdvanced(version:Long, insWits:Seq[(TxIn, TxWit)], outs:Seq[TxOut], lockTime:Long) = {    // inputs also contains amount
    val (ins, wits) = insWits.unzip
    val inBytes = ins.flatMap{in =>
      val prevTxHashBytes = toggleEndianString(in.txHash)
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
    val versionBytes = getFixedIntBytes(version, 4)
    val lockTimeBytes = getFixedIntBytes(lockTime, 4) // should be Seq[Byte](0x00, 0x00, 0x00, 0x00)
    versionBytes ++ flagMarkerBytes ++ inCtrBytes ++ inBytes ++ outCtrBytes ++ outBytes ++ witBytes ++ lockTimeBytes
  }.toArray
  
}
