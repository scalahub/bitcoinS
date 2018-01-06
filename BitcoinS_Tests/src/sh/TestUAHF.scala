package sh

import sh.ecc._
import sh.btc.DataStructures._
import sh.btc._
import sh.ecc.Util._
import sh.util.BytesUtil._

object TestUAHF1 extends App {
  BitcoinS.isMainNet = false
  val int = BigInt("谺酽⿄밑艧鯌⤡鍪渃溰鬔봬渹".getBytes)
  val prvKey = new PrvKey_P2PKH_UAHF(new ECCPrvKey(int mod n, true /* compressed */), false /* testnet */)
  val address = prvKey.pubKey.address // mgysA6VHQGAiojCVZjv71rKXAKkuBhqHKf
  assert(address == "mgysA6VHQGAiojCVZjv71rKXAKkuBhqHKf")
  // cc34790086a16e6d4d1f059b260cc8756869ada3ce5a5255cdb0654a460f3cab : 0 : 1300000000 satoshis
  // 3fc061b4df5b8cae2497f9733e5fab522cc81d3763c9417d61263d08f262c6a1 : 0 :  650000000 satoshis
  // 
  // total 19.5 BCH
  // output1: mgysA6VHQGAiojCVZjv71rKXAKkuBhqHKf : 3.49
  // output2: mgRoeWs2CeCEuqQmNfhJjnpX8YvtPACmCX : 10
  // output3: mgysA6VHQGAiojCVZjv71rKXAKkuBhqHKf : 3
  // output4: mgRoeWs2CeCEuqQmNfhJjnpX8YvtPACmCX : 3

  val in1 = TxIn("cc34790086a16e6d4d1f059b260cc8756869ada3ce5a5255cdb0654a460f3cab", 0)
  val in2 = TxIn("3fc061b4df5b8cae2497f9733e5fab522cc81d3763c9417d61263d08f262c6a1", 0)
  val out1 = new TxOut("mgysA6VHQGAiojCVZjv71rKXAKkuBhqHKf",  349000000) // this address
  val out2 = new TxOut("mgRoeWs2CeCEuqQmNfhJjnpX8YvtPACmCX", 1000000000) // faucet address
  val out3 = new TxOut("mgysA6VHQGAiojCVZjv71rKXAKkuBhqHKf",  300000000) // this address
  val out4 = new TxOut("mgysA6VHQGAiojCVZjv71rKXAKkuBhqHKf",  300000000) // this address
  val unsigned = BitcoinS.createNonSegWitTxRaw(Seq(in1, in2), Seq(out1, out2, out3, out4))
  val values = Seq((0, BigInt(1300000000)), (1, BigInt(650000000)))
  val signed = prvKey.signTx(unsigned, values)
  assert(signed.encodeHex == UASH_TestVectors1.signedHex)
  assert(new TxParser(signed).getTx.txid == "e3a16781936ffdc063444d1ecc209dfc786f8e37a4be5fb0ba76db0da6f3ea72")
  println("UAHF test 1 passed")
  TestUAHF2
}
object UASH_TestVectors1 {
  val signedHex = "0100000002ab3c0f464a65b0cd55525acea3ad696875c80c269b051f4d6d6ea186007934cc000000006a47304402204de7ba9fc7b792b2be5cc2a18f86a014488d889ea2015ff2455dc7bae749e1c902200f87fea391071b5b0c01ea03c71f81699d4e012788b3a9a7f6b9ca3926487b4b412102a4386dda1d2184a1bb4b1e39083d181c72dc867e1c1c44181ee6de25128f7e5bffffffffa1c662f2083d26617d41c963371dc82c52ab5f3e73f99724ae8c5bdfb461c03f000000006a47304402206dfb730b99bb2de50935a206fba70ad3ac560367b6b0da59bec4b63459369d57022066ce1d4f80bfdde15ce0b9345e953bc21a07982a37b92daded24400c667ee53a412102a4386dda1d2184a1bb4b1e39083d181c72dc867e1c1c44181ee6de25128f7e5bffffffff044051cd14000000001976a914100f1183ec54da113f1b83fc8378d63578a3e94988ac00ca9a3b000000001976a91409fed3e08e624b23dbbacc77f7b2a39998351a6888ac00a3e111000000001976a914100f1183ec54da113f1b83fc8378d63578a3e94988ac00a3e111000000001976a914100f1183ec54da113f1b83fc8378d63578a3e94988ac00000000"
  /* Below is the hash preimage computation:
    -------------------------------------
     input 1 hash preimage below
    single line:  0100000060eb165e7bbb4e6f3b61b1ea287a8ee851224305ab30689285bccb72152b866c752adad0a7b9ceca853768aebb6965eca126a62965f698a0c1bc43d83db632adab3c0f464a65b0cd55525acea3ad696875c80c269b051f4d6d6ea186007934cc000000001976a914100f1183ec54da113f1b83fc8378d63578a3e94988ac006d7c4d00000000ffffffffc2c0356468211aa5e3a149642b055a751275059f10bd3605a05265ced0700bbc0000000041000000
    DETAILS: 
    nVersion:     01000000
    hashPrevouts: 60eb165e7bbb4e6f3b61b1ea287a8ee851224305ab30689285bccb72152b866c
    hashSequence: 752adad0a7b9ceca853768aebb6965eca126a62965f698a0c1bc43d83db632ad
    outpoint:     ab3c0f464a65b0cd55525acea3ad696875c80c269b051f4d6d6ea186007934cc00000000
    scriptCode:   1976a914100f1183ec54da113f1b83fc8378d63578a3e94988ac
    amount:       006d7c4d00000000
    nSequence:    ffffffff
    hashOutputs:  c2c0356468211aa5e3a149642b055a751275059f10bd3605a05265ced0700bbc
    nLockTime:    00000000
    nHashType:    41000000
    -------------------------------------
     input 2 hash preimage below
    single line:  0100000060eb165e7bbb4e6f3b61b1ea287a8ee851224305ab30689285bccb72152b866c752adad0a7b9ceca853768aebb6965eca126a62965f698a0c1bc43d83db632ada1c662f2083d26617d41c963371dc82c52ab5f3e73f99724ae8c5bdfb461c03f000000001976a914100f1183ec54da113f1b83fc8378d63578a3e94988ac8036be2600000000ffffffffc2c0356468211aa5e3a149642b055a751275059f10bd3605a05265ced0700bbc0000000041000000
    DETAILS:     
                  01000000
                  60eb165e7bbb4e6f3b61b1ea287a8ee851224305ab30689285bccb72152b866c
                  752adad0a7b9ceca853768aebb6965eca126a62965f698a0c1bc43d83db632ad
                  a1c662f2083d26617d41c963371dc82c52ab5f3e73f99724ae8c5bdfb461c03f00000000
                  1976a914100f1183ec54da113f1b83fc8378d63578a3e94988ac
                  8036be2600000000
                  ffffffff
                  c2c0356468211aa5e3a149642b055a751275059f10bd3605a05265ced0700bbc
                  00000000
                  41000000

    Signed hex: 0100000002ab3c0f464a65b0cd55525acea3ad696875c80c269b051f4d6d6ea186007934cc000000006a47304402204de7ba9fc7b792b2be5cc2a18f86a014488d889ea2015ff2455dc7bae749e1c902200f87fea391071b5b0c01ea03c71f81699d4e012788b3a9a7f6b9ca3926487b4b412102a4386dda1d2184a1bb4b1e39083d181c72dc867e1c1c44181ee6de25128f7e5bffffffffa1c662f2083d26617d41c963371dc82c52ab5f3e73f99724ae8c5bdfb461c03f000000006a47304402206dfb730b99bb2de50935a206fba70ad3ac560367b6b0da59bec4b63459369d57022066ce1d4f80bfdde15ce0b9345e953bc21a07982a37b92daded24400c667ee53a412102a4386dda1d2184a1bb4b1e39083d181c72dc867e1c1c44181ee6de25128f7e5bffffffff044051cd14000000001976a914100f1183ec54da113f1b83fc8378d63578a3e94988ac00ca9a3b000000001976a91409fed3e08e624b23dbbacc77f7b2a39998351a6888ac00a3e111000000001976a914100f1183ec54da113f1b83fc8378d63578a3e94988ac00a3e111000000001976a914100f1183ec54da113f1b83fc8378d63578a3e94988ac00000000
   */
}
object TestUAHF2 {
  BitcoinS.isMainNet = false
  val int = BigInt("谺酽⿄밑艧鯌⤡鍪渃溰鬔봬渹".getBytes)
  val prvKey = new PrvKey_P2PKH_UAHF(new ECCPrvKey(int mod n, true /* compressed */), false /* testnet */)
  val address = prvKey.pubKey.address // mgysA6VHQGAiojCVZjv71rKXAKkuBhqHKf
  assert(address == "mgysA6VHQGAiojCVZjv71rKXAKkuBhqHKf")
  // e3a16781936ffdc063444d1ecc209dfc786f8e37a4be5fb0ba76db0da6f3ea72 : 0 :  349000000 satoshis
  // e3a16781936ffdc063444d1ecc209dfc786f8e37a4be5fb0ba76db0da6f3ea72 : 2 :  300000000 satoshis
  // e3a16781936ffdc063444d1ecc209dfc786f8e37a4be5fb0ba76db0da6f3ea72 : 3 :  300000000 satoshis
  // 
  // total 9.49 BCH
  // output1: mgRoeWs2CeCEuqQmNfhJjnpX8YvtPACmCX : 9.48

  val in1 = TxIn("e3a16781936ffdc063444d1ecc209dfc786f8e37a4be5fb0ba76db0da6f3ea72", 0)
  val in2 = TxIn("e3a16781936ffdc063444d1ecc209dfc786f8e37a4be5fb0ba76db0da6f3ea72", 2)
  val in3 = TxIn("e3a16781936ffdc063444d1ecc209dfc786f8e37a4be5fb0ba76db0da6f3ea72", 3)
  val out1 = new TxOut("mgRoeWs2CeCEuqQmNfhJjnpX8YvtPACmCX",  948000000) // faucet address
  val unsigned = BitcoinS.createNonSegWitTxRaw(Seq(in1, in2, in3), Seq(out1))
  val values = Seq(
    (0, BigInt(349000000)), 
    (1, BigInt(300000000)), 
    (2, BigInt(300000000))
  )
  val signed = prvKey.signTx(unsigned, values)
  assert(signed.encodeHex == UASH_TestVectors2.signedHex)
  assert(new TxParser(signed).getTx.txid == "c4e05e92c867f56ba846b45f42cbbc335d1147884c62e29035609a0b9756dd6e")
  println("UAHF test 2 passed")
}
object UASH_TestVectors2 {
  val signedHex = "010000000372eaf3a60ddb76bab05fbea4378e6f78fc9d20cc1e4d4463c0fd6f938167a1e3000000006a47304402203256c2d3d71dfe8fa2202cdc0628b4054eaca50d0f64931410e3a28fb4b63996022008854af8d3a7f960bde328dfa1f7e698541c1af0078753a9afe6a77608a46b44412102a4386dda1d2184a1bb4b1e39083d181c72dc867e1c1c44181ee6de25128f7e5bffffffff72eaf3a60ddb76bab05fbea4378e6f78fc9d20cc1e4d4463c0fd6f938167a1e3020000006b483045022100e8ccbd510e3559129acc9b608e223aaf8b1e6874745f5e8c1ddee539d94d555702203d7f131a2419c788f08ef1efcafbd5d75379a6dbd40be67501d2f4f75fbd7076412102a4386dda1d2184a1bb4b1e39083d181c72dc867e1c1c44181ee6de25128f7e5bffffffff72eaf3a60ddb76bab05fbea4378e6f78fc9d20cc1e4d4463c0fd6f938167a1e3030000006a47304402200316c6770660631431346ed4814b59c014b0c1f1f8aec36042ced72511877cdd02201716386f5ac683225e2f038de945e820167117a17a834473d2cf821d906ff3be412102a4386dda1d2184a1bb4b1e39083d181c72dc867e1c1c44181ee6de25128f7e5bffffffff0100558138000000001976a91409fed3e08e624b23dbbacc77f7b2a39998351a6888ac00000000"
}
