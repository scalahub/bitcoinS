
package sh

import sh.btc.DataStructures._
import sh.btc._
import sh.util.Hex
import sh.btc.BitcoinUtil._
import sh.ecc.PrvKey
import sh.ecc.Util._

object TestSegWitSample extends App {
  // following uses the example at https://github.com/bitcoin/bips/blob/master/bip-0143.mediawiki#P2SHP2WPKH
  /*
The following is an unsigned transaction: 0100000001db6b1b20aa0fd7b23880be2ecbd4a98130974cf4748fb66092ac4d3ceb1a54770100000000feffffff02b8b4eb0b000000001976a914a457b684d7f0d539a46a45bbc043f35b59d0d96388ac0008af2f000000001976a914fd270b1ee6abcaea97fea7ad0402e8bd8ad6d77c88ac92040000
  
    nVersion:  01000000
    txin:      01 db6b1b20aa0fd7b23880be2ecbd4a98130974cf4748fb66092ac4d3ceb1a5477 01000000 00 feffffff
    txout:     02 b8b4eb0b00000000 1976a914a457b684d7f0d539a46a45bbc043f35b59d0d96388ac
                  0008af2f00000000 1976a914fd270b1ee6abcaea97fea7ad0402e8bd8ad6d77c88ac
    nLockTime: 92040000
  
  The input comes from a P2SH-P2WPKH witness program:
    scriptPubKey : a9144733f37cf4db86fbc2efed2500b4f4e49f31202387, value: 10
    redeemScript : 001479091972186c449eb1ded22b78e40d009bdf0089
    private key  : eb696a065ef48a2192da5b28b694f87544b30fae8327c4510137a922f32c6dcf
    public key   : 03ad1d8e89212f0b92c74d23bb710c00662ad1470198ac48c43f7d6f93a2a26873
  
  To sign it with a nHashType of 1 (SIGHASH_ALL):
  
  hashPrevouts:
    dSHA256(db6b1b20aa0fd7b23880be2ecbd4a98130974cf4748fb66092ac4d3ceb1a547701000000)
  = b0287b4a252ac05af83d2dcef00ba313af78a3e9c329afa216eb3aa2a7b4613a
  
  hashSequence:
    dSHA256(feffffff)
  = 18606b350cd8bf565266bc352f0caddcf01e8fa789dd8a15386327cf8cabe198
  
  hashOutputs:
    dSHA256(b8b4eb0b000000001976a914a457b684d7f0d539a46a45bbc043f35b59d0d96388ac0008af2f000000001976a914fd270b1ee6abcaea97fea7ad0402e8bd8ad6d77c88ac)
  = de984f44532e2173ca0d64314fcefe6d30da6f8cf27bafa706da61df8a226c83
  
  hash preimage: 01000000b0287b4a252ac05af83d2dcef00ba313af78a3e9c329afa216eb3aa2a7b4613a18606b350cd8bf565266bc352f0caddcf01e8fa789dd8a15386327cf8cabe198db6b1b20aa0fd7b23880be2ecbd4a98130974cf4748fb66092ac4d3ceb1a5477010000001976a91479091972186c449eb1ded22b78e40d009bdf008988ac00ca9a3b00000000feffffffde984f44532e2173ca0d64314fcefe6d30da6f8cf27bafa706da61df8a226c839204000001000000
  
    nVersion:     01000000
    hashPrevouts: b0287b4a252ac05af83d2dcef00ba313af78a3e9c329afa216eb3aa2a7b4613a
    hashSequence: 18606b350cd8bf565266bc352f0caddcf01e8fa789dd8a15386327cf8cabe198
    outpoint:     db6b1b20aa0fd7b23880be2ecbd4a98130974cf4748fb66092ac4d3ceb1a547701000000
    scriptCode:   1976a91479091972186c449eb1ded22b78e40d009bdf008988ac
    amount:       00ca9a3b00000000
    nSequence:    feffffff
    hashOutputs:  de984f44532e2173ca0d64314fcefe6d30da6f8cf27bafa706da61df8a226c83
    nLockTime:    92040000
    nHashType:    01000000
                
  sigHash:      64f3b0f4dd2bb3aa1ce8566d220cc74dda9df97d8490cc81d89d735c92e59fb6
  signature:    3044022047ac8e878352d3ebbde1c94ce3a10d057c24175747116f8288e5d794d12d482f0220217f36a485cae903c713331d877c1f64677e3622ad4010726870540656fe9dcb01
  
  The serialized signed transaction is: 01000000000101db6b1b20aa0fd7b23880be2ecbd4a98130974cf4748fb66092ac4d3ceb1a5477010000001716001479091972186c449eb1ded22b78e40d009bdf0089feffffff02b8b4eb0b000000001976a914a457b684d7f0d539a46a45bbc043f35b59d0d96388ac0008af2f000000001976a914fd270b1ee6abcaea97fea7ad0402e8bd8ad6d77c88ac02473044022047ac8e878352d3ebbde1c94ce3a10d057c24175747116f8288e5d794d12d482f0220217f36a485cae903c713331d877c1f64677e3622ad4010726870540656fe9dcb012103ad1d8e89212f0b92c74d23bb710c00662ad1470198ac48c43f7d6f93a2a2687392040000
    nVersion:  01000000
    marker:    00
    flag:      01
    txin:      01 db6b1b20aa0fd7b23880be2ecbd4a98130974cf4748fb66092ac4d3ceb1a5477 01000000 
               1716001479091972186c449eb1ded22b78e40d009bdf0089 
               feffffff
    txout:     02 b8b4eb0b00000000 1976a914a457b684d7f0d539a46a45bbc043f35b59d0d96388ac
                  0008af2f00000000 1976a914fd270b1ee6abcaea97fea7ad0402e8bd8ad6d77c88ac
    witness    02 473044022047ac8e878352d3ebbde1c94ce3a10d057c24175747116f8288e5d794d12d482f0220217f36a485cae903c713331d877c1f64677e3622ad4010726870540656fe9dcb01 
                  2103ad1d8e89212f0b92c74d23bb710c00662ad1470198ac48c43f7d6f93a2a26873
    nLockTime: 92040000
   */
  val unsigned = "0100000001db6b1b20aa0fd7b23880be2ecbd4a98130974cf4748fb66092ac4d3ceb1a54770100000000feffffff02b8b4eb0b000000001976a914a457b684d7f0d539a46a45bbc043f35b59d0d96388ac0008af2f000000001976a914fd270b1ee6abcaea97fea7ad0402e8bd8ad6d77c88ac92040000"
  val tx = new TxParserSegWit(unsigned).getSegWitTx
  require(tx.version == 1)
  require(tx.lockTime == getUInt4LittleEndian("92040000"))
  
  val prvKey = new PrvKey("eb696a065ef48a2192da5b28b694f87544b30fae8327c4510137a922f32c6dcf", true)
  val pubKey = prvKey.pubKey
  require(pubKey.pubKeyHex.toLowerCase == "03ad1d8e89212f0b92c74d23bb710c00662ad1470198ac48c43f7d6f93a2a26873")
  val redeemScript = pubKey.getRedeemScript_P2WPKH
  require(Hex.encodeBytes(redeemScript).toLowerCase == "001479091972186c449eb1ded22b78e40d009bdf0089")
  val address = pubKey.getAddress_P2WPKH
  val scriptPubKey = getScriptPubKeyFromAddress(address).toArray
  require(Hex.encodeBytes(scriptPubKey).toLowerCase == "a9144733f37cf4db86fbc2efed2500b4f4e49f31202387")
  
  val prevTxHash = Hex.encodeBytes(Hex.decode("db6b1b20aa0fd7b23880be2ecbd4a98130974cf4748fb66092ac4d3ceb1a5477").reverse)
  val prevVOut = getUInt4LittleEndian("01000000").toInt
  val in = In(prevTxHash, prevVOut).setSeqNum(BigInt("feffffff", 16).toLong)
  require(prevVOut == 1)
  require(in == tx.ins(0))
  val outAddr1 = getAddrFromOutScript("76a914a457b684d7f0d539a46a45bbc043f35b59d0d96388ac")
  val outAddr2 = getAddrFromOutScript("76a914fd270b1ee6abcaea97fea7ad0402e8bd8ad6d77c88ac")
  
  val amt1 = getUInt8LittleEndian("b8b4eb0b00000000")
  val amt2 = getUInt8LittleEndian("0008af2f00000000")
  
  val out1 = Out(outAddr1, amt1)
  val out2 = Out(outAddr2, amt2)
  require(out1 == tx.outs(0))
  require(out2 == tx.outs(1))
  // below assumes that the sample uses deterministic k generation used in RFC6969 (which it does!)
  // so the signed tx also matches!
  val signed = prvKey.signTx_P2SH_P2WPKH(unsigned, Seq((0, 1000000000)))
  require(Hex.encodeBytes(signed).toLowerCase == "01000000000101db6b1b20aa0fd7b23880be2ecbd4a98130974cf4748fb66092ac4d3ceb1a5477010000001716001479091972186c449eb1ded22b78e40d009bdf0089feffffff02b8b4eb0b000000001976a914a457b684d7f0d539a46a45bbc043f35b59d0d96388ac0008af2f000000001976a914fd270b1ee6abcaea97fea7ad0402e8bd8ad6d77c88ac02473044022047ac8e878352d3ebbde1c94ce3a10d057c24175747116f8288e5d794d12d482f0220217f36a485cae903c713331d877c1f64677e3622ad4010726870540656fe9dcb012103ad1d8e89212f0b92c74d23bb710c00662ad1470198ac48c43f7d6f93a2a2687392040000")
  val stx = new TxParserSegWit(signed).getSegWitTx
  println("signed txid "+stx.txid)
  println("signed hash "+stx.segWitTxHash)
  println("Passed")
}
