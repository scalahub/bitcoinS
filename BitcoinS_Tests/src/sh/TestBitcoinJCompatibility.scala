package sh

import java.math.BigInteger
import org.bitcoinj.core._
import org.bitcoinj.core.{Transaction => JTx}
import org.bitcoinj.core.{Address => JAddress}
import org.bitcoinj.core.{ECKey => JKey}
import org.bitcoinj.crypto.TransactionSignature
import org.bitcoinj.params._
import org.bitcoinj.script.ScriptBuilder
import sh.btc.BitcoinS._
import sh.btc.BitcoinS._
import sh.btc.DataStructures._
import sh.btc._
import sh.ecc._
import sh.ecc.Util._
import sh.util.BytesUtil
import sh.util.HashUtil
import sh.util.StringUtil

object TestBitcoinJCompatibility extends App {
  def hash(b:Array[Byte]) = {
    val d = java.security.MessageDigest.getInstance("SHA-256")
    d.digest(b)
  }
  val jNetParam = getJNetParams
  Seq(false, true).map{compressed => 
    1 to 20 foreach{i =>    
      val s = i.toString
      val h = hash(s.getBytes)
      val int = new BigInteger(h)
      val sKey = getKeyFromInt((int mod n).bigInteger, compressed)
      val jKey = ECKey.fromPrivate(int, compressed)
      val sAddr = sKey.pubKey.address
      val jAddr = jKey.toAddress(jNetParam).toString
      assert(sKey.pubKey.eccPubKey.hex.toLowerCase == jKey.getPublicKeyAsHex.toLowerCase, s"Key mismatch for int $int")
      assert(sAddr == jAddr, sAddr+" != "+jAddr)
      print(".")
    }
  }
  println
  println("BitcoinJ private key from int compatibility tests passed")
  // Create and sign tx. Ensure that results from BitcoinJ match the ones returned here.
  // Note that both unsigned and signed tx must match. The signed must match because both libraries use deterministic k generation from RFC6979
  // We can only P2PKH inputs since BitcoinJ does not yet support SegWit (P2SH_P2WPKH)
  import HashUtil._
  import StringUtil._
  import BytesUtil._
  Seq(false, true).map{compressed => 
    1 to 10 foreach{i =>    
      val ins = 1 to 20 map{i =>
        val eccKey = new ECCPrvKey(dsha256(("a"+i).getBytes).encodeHex, compressed)
        val key = new PrvKey_P2PKH(eccKey, isMainNet)
        val txid = dsha256(("b"+i).getBytes).encodeHex
        val vout = i
        val jKey = ECKey.fromPrivate(eccKey.bigInt.bigInteger, compressed)
        (key, TxIn(txid, vout).setSeqNum(0xFFFFFFFFL - 2), jKey)
      }
      val outs = 1 to 40 map{i =>
        val seed = dsha256(("c"+i).getBytes)
        val amt =  BigInt(seed.takeRight(5).encodeHex, 16)
        val eccKey = new ECCPrvKey(dsha256(("d"+i).getBytes).encodeHex, compressed)
        val addr = seed(0) match {
          case a if a > 43 => new PrvKey_P2PKH(eccKey, isMainNet).pubKey.address
          case a if a > -43 => new PrvKey_P2SH_P2PK(eccKey, isMainNet).pubKey.address
          case _ => new PrvKey_P2SH_P2WPKH(eccKey.bigInt, isMainNet).pubKey.address
        }
        new TxOut(addr, amt)
      }
      val sTx = createNonSegWitTxRaw(ins.unzip3._2, outs)
      val jTx = createJTx(ins.unzip3._2, outs)
      assert(jTx.bitcoinSerialize.encodeHex == sTx.encodeHex)
      val ssTx = ins.unzip3._1.zipWithIndex.foldLeft(sTx){case (oldTx, (key, i)) => key.signTx(oldTx, i, 10000000000000L)} 
      val sjTx = signJTx(jTx, ins.unzip3._3)
      assert(sjTx.bitcoinSerialize.encodeHex == ssTx.encodeHex)
      print(".")
    }
  }
  println
  println("Create and sign tx tests passed")

  def createJTx(ins:Seq[TxIn], outs:Seq[TxOut]) = {        
    val t = new JTx(getJNetParams)  
    outs.foreach(out => t.addOutput(Coin.valueOf(out.value.toLong), new JAddress(getJNetParams, out.optAddress.get)))
    ins.foreach{in => 
      val input = new TransactionInput(getJNetParams, t, Array[Byte](), new TransactionOutPoint(getJNetParams, in.vOut.toLong, new Sha256Hash(in.txHash)))
      input.setSequenceNumber(0xFFFFFFFFL - 2)
      t.addInput(input)
    }
    t
  }
  def signJTx(t:JTx, keys:Seq[JKey]) = {
    keys.zipWithIndex.map{
      case (k, i) =>
        (k.sign(t.hashForSignature(i, ScriptBuilder.createOutputScript(k.toAddress(getJNetParams)), JTx.SigHash.ALL, false)), i, k)
    }.map{case (sig, i, key) =>
      t.getInput(i).setScriptSig(ScriptBuilder.createInputScript(new TransactionSignature(sig, JTx.SigHash.ALL, false), key));
    }
    t
  }
  def getKeyFromInt(int: java.math.BigInteger, compressed:Boolean) = new PrvKey_P2PKH(ECCPrvKey(int, compressed), isMainNet)
  def getJNetParams = if (isMainNet) new MainNetParams else new TestNet3Params
}
