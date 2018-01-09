
package sh.btc

import sh.ecc._
import sh.ecc.Util._
import sh.btc.BitcoinS._
import sh.btc.BitcoinUtil._
import sh.util.StringUtil._
import sh.util.HashUtil._

abstract class PubKey(val eccPubKey:ECCPubKey, val mainNet:Boolean) {
  val address:String
  val bytes = eccPubKey.bytes
  val doubleHashedPubKeyBytes = ripeMD160(sha256Bytes2Bytes(bytes))
  val redeemScript:Seq[Byte]
}

class PubKey_P2PKH(eccPubKey:ECCPubKey, mainNet:Boolean) extends PubKey(eccPubKey, mainNet) {
  /*  Details:
      https://en.bitcoin.it/wiki/Technical_background_of_version_1_Bitcoin_addresses
      https://bitcoin.stackexchange.com/a/3839/2075
      https://en.bitcoin.it/wiki/Base58Check_encoding#Version_bytes  */
  lazy val redeemScript = Nil // should not be needed for P2PKH
  lazy val address = {
    val hash = doubleHashedPubKeyBytes 
    val addrBytes = if (mainNet) 0x00.toByte +: hash else 111.toByte +: hash
    getBase58FromBytes(addrBytes)
  }
}
class PubKey_P2SH_P2PK(eccPubKey:ECCPubKey, mainNet:Boolean) extends PubKey(eccPubKey, mainNet) {
  /*  https://github.com/bitcoin/bips/blob/master/bip-0016.mediawiki
      For example, the scriptPubKey and corresponding scriptSig for a one-signature-required transaction is:
      scriptSig: [signature] {[pubkey] OP_CHECKSIG}
      scriptPubKey: OP_HASH160 [20-byte-hash of {[pubkey] OP_CHECKSIG} ] OP_EQUAL
    
      Pubkey script: OP_HASH160 <Hash160(redeemScript)> OP_EQUAL
      Signature script: <sig> [sig] [sig...] <redeemScript>   
   
      public key will be either 65 (uncompressed) or 33 (compressed)
      Thus, size of pubKey is <= 75 and can be represented in one byte (pushdata)  */
  lazy val redeemScript = Seq(eccPubKey.bytes.size.toByte)++eccPubKey.bytes++Seq(OP_CheckSig)
  
  lazy val address = { // simple 1 out of 1 P2SH from BIP16
    val redeemScriptHash = hash160(redeemScript.toArray)
    val addrBytes = (if (mainNet) 0x05.toByte else 0xC4.toByte) +: redeemScriptHash
    getBase58FromBytes(addrBytes) 
  }
}
class PubKey_P2SH_P2WPKH (point:Point, mainNet:Boolean) extends PubKey(ECCPubKey(point, true), mainNet) {
  lazy val redeemScript:Seq[Byte] = "0014".decodeHex ++ doubleHashedPubKeyBytes   // example 00147646c030f7e75b80f0a31cdcab731e6f424f22b2
  /*  To create a P2SH-P2WPKH address:

      Calculate the RIPEMD160 of the SHA256 of a public key (keyhash). 

      The P2SH redeemScript is always 22 bytes. 
      It starts with a OP_0, followed by a canonical push of the keyhash 
      (i.e. 0x0014{20-byte keyhash})

      Same as any other P2SH, the scriptPubKey is 
      OP_HASH160 hash160(redeemScript) OP_EQUAL,  
      and the address is the corresponding P2SH address with prefix 3.  

      The scriptPubKey is the locking script, and is NOT used in computation of the address itself!   */
  
  lazy val address = {
  /*  Test vector from: https://bitcoin.stackexchange.com/q/60961/2075
      
      Public key - compressed: 
      03fac6879502c4c939cfaadc45999c7ed7366203ad523ab83ad5502c71621a85bb

      SHA256(public key) =
      cfad24b0bc2bba2c8bb2c8d619dca2b74221930793bca50df73856f0bbba10c9

      RIPEMD160(SHA256(public key)) =
      7646c030f7e75b80f0a31cdcab731e6f424f22b2

      redeemScript (OP_0 pubkeyHash160):
      00147646c030f7e75b80f0a31cdcab731e6f424f22b2

      SHA256(redeemScript) =
      a10e523968ba784d24ccd54e613d8f747d6649e42b1df4fdcec6658262620651

      RIPEMD160(SHA256(redeemScript)) =
      188ba16284702258959d8bb63bb9a5d979b57875

      Then do BitcoinUtil.getBase58FromBytes(above value)   */
    val redeemScriptHash = hash160(redeemScript) 
    val addrBytes = (if (mainNet) 0x05.toByte else 196.toByte) +: redeemScriptHash
    getBase58FromBytes(addrBytes)
  }
}
