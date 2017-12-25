package sh


import sh.ecc._
import sh.btc._
import sh.util.Hex
import sh.btc.DataStructures._
import sh.btc.BitcoinUtil._

object TestSegWit extends App {
  // creates tx with mixed inputs (segwit, p2pkh and p2sh-p2pk) types as examples
  isMainNet = false // set to testnet
  
  val key = new sh.ecc.PrvKey("BB2AC60BC518C0E239D5AF9D8D051A6BDFD0D931268DCA70C59E5992", true)   // random key
  
  println ("Segwit (P2SH-P2WPKH) address: "+key.pubKey.getAddress_P2WPKH) // 2N6nA4btbY23GTQ4RJi3mMGTonzXN7dbphE   (segwit)
  println ("Non-Segwit (P2SH-P2PK) address: "+key.pubKey.getAddress_P2SH_P2PK) // 2MwprvB9tUMtX4vK8zJK8K329fNu79CJgR7   (p2sh)
  println ("Non-Segwit (P2PKH) address: "+key.pubKey.getAddress) // muLwLjVipwixXcECVMcEwgsrtfGRTB4zB1    (p2pkh)
  assert(key.pubKey.getAddress_P2WPKH == "2N6nA4btbY23GTQ4RJi3mMGTonzXN7dbphE")
  assert(key.pubKey.getAddress_P2SH_P2PK == "2MwprvB9tUMtX4vK8zJK8K329fNu79CJgR7")
  assert(key.pubKey.getAddress == "muLwLjVipwixXcECVMcEwgsrtfGRTB4zB1")
  
  // Send some coins to the above addresses. During testing, the following coins were used:
  val in0 = In("0224c8875a43c482c81a508fafc10bd0f084b27b5543c228e48431985f321547", 0) // p2pkh
  val in1 = In("63bec90405a0c173ae928860a1e1d403e507ec225e2cdd07717c8408820d418b", 0) // segwit // 2031250 satoshis
  val in2 = In("db5a23f0da2502b8ef82d93453aa3be0b6e9a3ecfbfbc00818dc32b3c712d2d0", 0) // p2pkh
  val in3 = In("6d73e3c8f66869859dc5c1ba73f252b8db51950ebc1fc4a59dca3552a0085f9a", 0) // p2sh
  val in4 = In("6ce466eb0920f84cc2cfde1d359176c0baab7dc442e4e5763bf67e8fa96ee6a4", 0) // p2sh
  val in5 = In("b49f3d6d15f2bdd9217ba3caaf1bb1f2d9875c9657e6b0ac7a0ef841d486ad1d", 2) // p2pkh
  val in6 = In("b75dfb27477834b3060c8e956273bef81c62689a8b9ebb7cd4a8119508c2c798", 0) // segwit // 1015625 satoshis

  // the following output was used to test
  // Note 2N8hwP1WmJrFF5QWABn38y63uYLhnJYJYTF faucet address
  val out1 = Out(Some("2N8hwP1WmJrFF5QWABn38y63uYLhnJYJYTF"), 32206092) // total amount is slightly more than 32206092
  
  
  val tx0 = createSegWitTxRaw(Seq(in0,in1,in2,in3,in4,in5,in6), Seq(out1)) // unsigned tx
  val tx1 = key.signTx_P2PKH (tx0, Seq(0, 2, 5)) // inputs 0, 2, 5 are P2PKH 
  val tx2 = key.signTx_P2SH_P2WPKH(
    tx1, Seq(
      (1, 2031250), 
      (6, 1015625)
    )
  )  // inputs 1, 6 are segwit. Those need the input value in satoshis as well
  
  val signed = key.signTx_P2SH_P2PK(tx2, Seq(3, 4)) // inputs 3, 4 are P2SH_P2PK
  
  
  val parsed = new TxParserSegWit(signed).getSegWitTx
  println("Hex: "+Hex.encodeBytes(signed))
  println("txid: "+parsed.txid) // tx is signed // should print 
  println("hash: "+parsed.segWitTxHash)  // tx is signed
  /*  Above tx was sent with txID c2e205d632d589b0a8c52fb71d7ecf35e112b241dca4a8e6d6b411764a630a28
      https://www.blocktrail.com/tBTC/tx/c2e205d632d589b0a8c52fb71d7ecf35e112b241dca4a8e6d6b411764a630a28

      However during the initial test, we did not use deterministic k values of signature. So the id now will be different!
      Current is as follows:
      txid: 3e1efea9ebc892d3c1d5fbc35fba973b42c819ce85fce74a435a2729674d4f0e
      hash: 596e904f07f3c5deaafdfa0d078c103e50957a804426e0c229775eb4085afeb1  */
}

object TestP2PKH extends App { 
  // standard tx (most common till now). Sent to 1abc address (mainet) or mabc.. /nabc.. address (testnet)
  isMainNet = false // set to testnet
  val key = new PrvKey("BB2AC60BC518C0E239D5AF9D8D051A6BDFD0D931268DCA70C59E5992", true)
  println("P2PKH address "+key.pubKey.getAddress) // muLwLjVipwixXcECVMcEwgsrtfGRTB4zB1 
  assert(key.pubKey.getAddress == "muLwLjVipwixXcECVMcEwgsrtfGRTB4zB1")
  // following inputs were used in creating tx (funds were sent to above muLwLjVipwixXcECVMcEwgsrtfGRTB4zB1)
  val in1 = In("56585700f02e26d8d1f71634edbdab74b4eba06abbf55bb918352ed7d4942e0d", 0)
  val in2 = In("5ef0bea6abe1203f8521f61e2040024c39f48f7253df9ee3b919a8513bb01c36", 0)

  // the following output was used to test
  // Note 2N8hwP1WmJrFF5QWABn38y63uYLhnJYJYTF faucet address
  val out1 = Out(Some("2N8hwP1WmJrFF5QWABn38y63uYLhnJYJYTF"),  48700000)
  val tx = BitcoinUtil.createNonSegWitTxRaw(Seq(in1, in2), Seq(out1))
  val signed = key.signTx_P2PKH(tx, Seq(0, 1)) // inputs 0 and 1 are P2PKH inputs
  val parsed = new TxParser(signed).getTx
  println("Hex: "+Hex.encodeBytes(signed)  )
  println("txid: "+parsed.txid)
}

object TestP2SH_P2PK extends App {
  // uses P2SH_P2PK described in https://github.com/bitcoin/bips/blob/master/bip-0016.mediawiki
  isMainNet = false // set to testnet
  val key = new sh.ecc.PrvKey("BB2AC60BC518C0E239D5AF9D8D051A6BDFD0D931268DCA70C59E5992", true)
  println("P2SH_P2PK address "+key.pubKey.getAddress_P2SH_P2PK) //2MwprvB9tUMtX4vK8zJK8K329fNu79CJgR7
  assert(key.pubKey.getAddress_P2SH_P2PK == "2MwprvB9tUMtX4vK8zJK8K329fNu79CJgR7")
  
  println("P2PKH address "+key.pubKey.getAddress) //muLwLjVipwixXcECVMcEwgsrtfGRTB4zB1
  assert(key.pubKey.getAddress == "muLwLjVipwixXcECVMcEwgsrtfGRTB4zB1")
  
  // following inputs were used to create tx
  val in2 = In("d9524c287a4f6d6f072244faab539f1ed8541b557bd904a3f56f51f371786a0f", 0) // 0.34000000 // 2MwprvB9tUMtX4vK8zJK8K329fNu79CJgR7
  val in3 = In("a1ba359c11255e366e1a0adf6a3d8fd148960468114913779c1420919d382591", 0) // 1.3 // muLwLjVipwixXcECVMcEwgsrtfGRTB4zB1
  val in4 = In("5e5628b2f631955a425deae64da883d36b003ce75e668b7e5b12988e6dca55f4", 0) // 0.08125000 // muLwLjVipwixXcECVMcEwgsrtfGRTB4zB1

  // the following output was used to test
  // Note 2N8hwP1WmJrFF5QWABn38y63uYLhnJYJYTF faucet address
  val out2 = Out(Some("2N8hwP1WmJrFF5QWABn38y63uYLhnJYJYTF"),  172000000)
  val tx = BitcoinUtil.createNonSegWitTxRaw(Seq(in2, in3, in4), Seq(out2))
  val tx1 = key.signTx_P2SH_P2PK(tx, Seq(0))
  val tx2 = key.signTx_P2PKH(tx1, Seq(1, 2))
  val signed = Hex.encodeBytes(tx2)  
  val parsed = new TxParser(tx2).getTx
  println("Hex: "+signed)
  println("txid: "+parsed.txid)
}
