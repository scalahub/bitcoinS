
package sh

import sh.btc._
import sh.ecc.Util._
import scala.collection.JavaConversions._
import sh.util._
import sh.btc.BitcoinUtil._

object TxParserTest extends App {
  import TxParserTestVectors._
  Seq(
    coinBaseTx, 
    dataTx, 
    coinbaseBurnTx, 
    randomTx,
    segWitTx1,
    segWitTx2,
    segWitTx3,
    segWitTx4
  ).foreach{
    case (hex, txid, hash, vSize) => // hash is segwitTxID
      new TxParserTest(hex, txid, hash, vSize)
  }
  println("All tx parser tests passed!")
}
class TxParserTest(hex:String, txid:String, hash:String, vSize:Int) {
  val size = hex.size/2 
  val bytes = hex.decodeHex

  val tx = new TxParserSegWit(bytes).getSegWitTx
  val txBytes = tx.serialize

  assert(tx.vSize == vSize, s"TxVSize. Found: ${tx.vSize}. Expected: ${vSize}")
  assert(txBytes.size == size, s"TxBytesSize. Found: ${txBytes.size}. Expected: ${size}")
  assert(tx.size == size, s"TxSize. Found: ${tx.size}. Expected ${size}")
  assert(tx.txid == txid, s"TxId. Found ${tx.txid}. Expected ${txid}")
  assert(tx.segWitTxHash == hash, s"TxHash. Found ${tx.segWitTxHash}. Expected ${hash}")
  assert(txBytes.size == bytes.size, s"TxBytesSize. Found ${txBytes.size}. Expected ${bytes.size}")
  
  (txBytes zip bytes).zipWithIndex.foreach{
    case ((l, r), i) => assert(l == r, s"Left ($l) != Right ($r) at index $i")
  }

  val newBytes = createSegWitTxRawAdvanced(tx.version, tx.ins zip tx.witnesses, tx.outs, tx.lockTime)
  assert(newBytes.size == bytes.size)
  (newBytes zip bytes).zipWithIndex.foreach{
    case ((l, r), i) => assert(l == r, s"Left ($l) != Right ($r) at index $i")
  }
  
  val newTx = new TxParserSegWit(newBytes).getSegWitTx
  val newTxBytes = newTx.serialize
  assert(newTx.txid == tx.txid, s"Computed txid ${newTx.txid} != ${tx.txid}")
  assert(newTx.segWitTxHash == tx.segWitTxHash)
  assert(newTx.size == tx.size)
  assert(newTx.version == tx.version)
  assert(newTx.vSize == tx.vSize)
  println(s"Tx test passed for $txid")
}
object TxParserTestVectors {
  val coinBaseTx = (// coinbase tx
    "01000000010000000000000000000000000000000000000000000000000000000000000000ffffffff1b0356770506cbcde1b6e3fb084e8b873474fe192306457336e3ffceffffffff0121430696000000001976a9147f8723c3a5e64d6e1d47511863aca2f146b0a85588ac00000000",
    "9a223b09449cdd383206db29fe7f5ea31e0154d9fb30eabafe36eb8a9a33d22f", 
    "9a223b09449cdd383206db29fe7f5ea31e0154d9fb30eabafe36eb8a9a33d22f", 
    112
  )
  
  val dataTx = (// contains data (OP_Return)
    "01000000013b27df80c94895f426f2a83de8394f385dbf2d3ba3bd1945d88978397086242a020000008a4730440220099517d79515f27783e5e39981986ba87d54f29c3c54ca8ba18090172985b125022073700a3860e40ed534358bbb8a624eacb2804a36f4b3ab92e0d720c50a144549014104fc49b907b56beec9b810692456297db5a75521ea3614feb2bf77730e8517c254b114013f363b4f643e2f9cd0a7233ae240f5f8904052476be2d6d6fe66325456ffffffff010000000000000000246a225331a04d8f666b8dc3a331402438cf32057c4176950cef92cac63a4b3612d7ce91a500000000",
    "b9a36957d981d927307f7af07fe1f33823a73278fa5657cd247911b04d141ee5",
    "b9a36957d981d927307f7af07fe1f33823a73278fa5657cd247911b04d141ee5",
    234
  )
  
  val randomTx = (// had an error initially
    "0100000001507b2a307643dfde8780ddd5ab1e959effd6cd886e4c63b076e36e4eea7e1ee507000000fdff000048304502206d2179d8918c479268cbd45a38fe730b1303eb9cea17c1960f5ed983cf6524120221009b6812909567c064681cd6afc629aeccebcfa67eaa1fbdbd2b221f5510b269d901493046022100a869fd885752c1296c9a7124b3e8064a4dda4bb82dfe516aaebc1b9b2e59dee6022100ec5ebda8cde9c9e1a2de28a8ae04a4b85dcfb0549994b6248941301210c66a98014c6952210233137e14a3de838740face22048958afa84860ddf2c98b49b217a3d3ad1e01bc210346eaacb853dc05f85156696996d32d19068d99009d3a234b89ce57b648c1e64a2103f6ac742f91d46dacf2d1fe1fb42734b0faff9b4f7abbad762fa30f6a375f603a53aeffffffff0f00e1f505000000001976a914b08f46e4d21cd0547a8a1e2e43e5440284f710a488acd07cac29000000001976a914b08f46e4d21cd0547a8a1e2e43e5440284f710a488aca08b87e7000000001976a914b08f46e4d21cd0547a8a1e2e43e5440284f710a488ac404ae314000000001976a914b08f46e4d21cd0547a8a1e2e43e5440284f710a488ac2048531c000000001976a914b08f46e4d21cd0547a8a1e2e43e5440284f710a488acc044334f000000001976a914b08f46e4d21cd0547a8a1e2e43e5440284f710a488ac50231923000000001976a914b08f46e4d21cd0547a8a1e2e43e5440284f710a488acb0d71c24000000001976a914b08f46e4d21cd0547a8a1e2e43e5440284f710a488ac309bb119000000001976a914b08f46e4d21cd0547a8a1e2e43e5440284f710a488ac30d64c02000000001976a914b08f46e4d21cd0547a8a1e2e43e5440284f710a488ac5013db11000000001976a914b08f46e4d21cd0547a8a1e2e43e5440284f710a488acc07e4e3b000000001976a914b08f46e4d21cd0547a8a1e2e43e5440284f710a488ac90cbb81d000000001976a914b08f46e4d21cd0547a8a1e2e43e5440284f710a488ac70a38f7e010000001976a914b08f46e4d21cd0547a8a1e2e43e5440284f710a488ace0f700c00700000017a914f4c25c8efe5e0443d22d5cb3a034a5088293d9698700000000",
    "a42dc25ead3237a07c5bde358febae04f84b5b357f47c4df1e69e108b46a02a6",
    "a42dc25ead3237a07c5bde358febae04f84b5b357f47c4df1e69e108b46a02a6",
    816
  )
  
  // https://blockchain.info/tx/9bf8853b3a823bbfa1e54017ae11a9e1f4d08a854dcce9f24e08114f2c921182
  // coin base with burn, no reward, size = 119. JSON below:
  /*
{
	"result": {
		"txid": "9bf8853b3a823bbfa1e54017ae11a9e1f4d08a854dcce9f24e08114f2c921182",
		"hash": "9bf8853b3a823bbfa1e54017ae11a9e1f4d08a854dcce9f24e08114f2c921182",
		"version": 1,
		"size": 119,
		"vsize": 119,
		"locktime": 0,
		"vin": [
			{
				"coinbase": "03dea707055a478cb801b80100006ea50000",
				"sequence": 4294967295
			}
		],
		"vout": [
			{
				"value": 0.00000000,
				"n": 0,
				"scriptPubKey": {
					"asm": "2 3 [error]",
					"hex": "52534b424c4f434b3addbf517adf8ffd4bca7751505b39c9013a0d1fd479fc4e901b39dd57b347c624",
					"type": "nonstandard"
				}
			}
		],
		"hex": "01000000010000000000000000000000000000000000000000000000000000000000000000ffffffff1203dea707055a478cb801b80100006ea50000ffffffff0100000000000000002952534b424c4f434b3addbf517adf8ffd4bca7751505b39c9013a0d1fd479fc4e901b39dd57b347c62400000000",
		"blockhash": "0000000000000000004b27f9ee7ba33d6f048f684aaeb0eea4befd80f1701126",
		"confirmations": 178,
		"time": 1514638520,
		"blocktime": 1514638520
	},
	"error": null,
	"id": null
}
*/
  val coinbaseBurnTx = (
    "01000000010000000000000000000000000000000000000000000000000000000000000000ffffffff1203dea707055a478cb801b80100006ea50000ffffffff0100000000000000002952534b424c4f434b3addbf517adf8ffd4bca7751505b39c9013a0d1fd479fc4e901b39dd57b347c62400000000",
    "9bf8853b3a823bbfa1e54017ae11a9e1f4d08a854dcce9f24e08114f2c921182",
    "9bf8853b3a823bbfa1e54017ae11a9e1f4d08a854dcce9f24e08114f2c921182",
    119
  )
  val segWitTx1 = ( // default implementers guide P2SH-P2WPKH (https://bitcoin.stackexchange.com/a/60894/2075)
    "01000000000101db6b1b20aa0fd7b23880be2ecbd4a98130974cf4748fb66092ac4d3ceb1a5477010000001716001479091972186c449eb1ded22b78e40d009bdf0089feffffff02b8b4eb0b000000001976a914a457b684d7f0d539a46a45bbc043f35b59d0d96388ac0008af2f000000001976a914fd270b1ee6abcaea97fea7ad0402e8bd8ad6d77c88ac02473044022047ac8e878352d3ebbde1c94ce3a10d057c24175747116f8288e5d794d12d482f0220217f36a485cae903c713331d877c1f64677e3622ad4010726870540656fe9dcb012103ad1d8e89212f0b92c74d23bb710c00662ad1470198ac48c43f7d6f93a2a2687392040000",
    "ef48d9d0f595052e0f8cdcf825f7a5e50b6a388a81f206f3f4846e5ecd7a0c23",
    "680f483b2bf6c5dcbf111e69e885ba248a41a5e92070cfb0afec3cfc49a9fabb",
    170 // size is 251
  )
  val segWitTx2 = ( // unsigned version of above (https://bitcoin.stackexchange.com/a/60894/2075)
    "0100000001db6b1b20aa0fd7b23880be2ecbd4a98130974cf4748fb66092ac4d3ceb1a54770100000000feffffff02b8b4eb0b000000001976a914a457b684d7f0d539a46a45bbc043f35b59d0d96388ac0008af2f000000001976a914fd270b1ee6abcaea97fea7ad0402e8bd8ad6d77c88ac92040000",
    "321a59707939041eeb0d524f34432c0c46ca3920f0964e6c23697581f176b6c0",
    "321a59707939041eeb0d524f34432c0c46ca3920f0964e6c23697581f176b6c0",
    119 // size is also 119 because its not signed yet, so size is same as a classic tx
  )
  val segWitTx3 = ( // should not understand this tx as uses Native P2WPKH input
    "01000000000101db6b1b20aa0fd7b23880be2ecbd4a98130974cf4748fb66092ac4d3ceb1a5477010000001716001479091972186c449eb1ded22b78e40d009bdf0089feffffff02b8b4eb0b000000001976a914a457b684d7f0d539a46a45bbc043f35b59d0d96388ac0008af2f000000001976a914fd270b1ee6abcaea97fea7ad0402e8bd8ad6d77c88ac02473044022047ac8e878352d3ebbde1c94ce3a10d057c24175747116f8288e5d794d12d482f0220217f36a485cae903c713331d877c1f64677e3622ad4010726870540656fe9dcb012103ad1d8e89212f0b92c74d23bb710c00662ad1470198ac48c43f7d6f93a2a2687392040000",
    "ef48d9d0f595052e0f8cdcf825f7a5e50b6a388a81f206f3f4846e5ecd7a0c23",
    "680f483b2bf6c5dcbf111e69e885ba248a41a5e92070cfb0afec3cfc49a9fabb",
    170 // size is 251
  )
  val segWitTx4 = ( // https://bitcoin.stackexchange.com/a/60895/2075
    "0200000000010140d43a99926d43eb0e619bf0b3d83b4a31f60c176beecfb9d35bf45e54d0f7420100000017160014a4b4ca48de0b3fffc15404a1acdc8dbaae226955ffffffff0100e1f5050000000017a9144a1154d50b03292b3024370901711946cb7cccc387024830450221008604ef8f6d8afa892dee0f31259b6ce02dd70c545cfcfed8148179971876c54a022076d771d6e91bed212783c9b06e0de600fab2d518fad6f15a2b191d7fbd262a3e0121039d25ab79f41f75ceaf882411fd41fa670a4c672c23ffaf0e361a969cde0692e800000000",
    "c586389e5e4b3acb9d6c8be1c19ae8ab2795397633176f5a6442a261bbdefc3a",
    "b759d39a8596b70b3a46700b83e1edb247e17ba58df305421864fe7a9ac142ea",
    134 // size is 216
  )
}
