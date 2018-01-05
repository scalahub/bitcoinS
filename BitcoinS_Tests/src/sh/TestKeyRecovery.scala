package sh

import sh.ecc._
import sh.ecc.Util._
import sh.btc.BitcoinUtil._
import sh.util.BytesUtil._
import sh.util.StringUtil._
import sh.util.BigIntUtil._
import sh.btc.BitcoinS._

object TestKeyRecovery extends App {
  TestKeyRecovery1
  TestKeyRecovery2
  TestKeyRecovery3
  TestKeyRecovery4
  TestKeyRecovery5
  TestKeyRecovery6
  TestKeyRecovery7
  println("All tests passed!")
}
object TestKeyRecovery1 {
  println("Test 1: [recover keys using method 1, validate signatures, custom signing method]")
  val a = new ECCPrvKey(100, true)
  val b = a.eccPubKey
  val c = a.sign("hello")
  val h = sha256Bytes2Bytes("hello".getBytes)
  val (r, s) = decodeDERSig(c)    
  val recovered = recoverPubKeyPoints(r, s, h)
  assert(recovered.size == 4)
  recovered.flatten.foreach{pk =>
    assert(pk.verify(h, r, s))
  }
}
object TestKeyRecovery2 {
  // Using the method in https://crypto.stackexchange.com/a/18106/81
  println("Test 2: [recover keys using method 2, validate signatures, custom signing method]")
  // Note that method 2 does not look at small r where r+n is also a point on the curve
  val a = new ECCPrvKey(100, true)
  val b = a.eccPubKey
  val c = a.sign("hello")
  //////////////////////////////////
  val h = sha256Bytes2Bytes("hello".getBytes)
  val (r, s) = decodeDERSig(c)    
  val z = BigInt(h.encodeHex, 16)
  val rInv = r.modInverse(n)

  val (ry1, ry2) = findYs(r)
  val R1 = new Point(r, ry1)
  val R2 = new Point(r, ry2)

  val P1:Point = rInv * (s * R1 - z * G)
  val P2:Point = rInv * (s * R2 - z * G)

  assert(b.point.verify(h, r, s) == true)
  assert(P1.verify(h, r, s) == true)
  assert(P2.verify(h, r, s) == true)
  
  val recovered = recoverPubKeyPoints(r, s, h)
  assert(recovered.size == 4)
  recovered.flatten.foreach{pk =>
    assert(pk.verify(h, r, s))
  }
  assert(recoverPubKeyPoints(r, s, h).flatten.contains(P1))
  assert(recoverPubKeyPoints(r, s, h).flatten.contains(P2))
}
object TestKeyRecovery3 {
  println("Test 3: [recover keys using method 1 with test vector, validate signatures, bitcoind signing method]")
  val msg = "18426974636F696E205369676E6564204D6573736167653A0A17736F206D616E7920746F2063686F6F73652066726F6D21".decodeHex
  // Bitcoin Signed Message: so many to choose from!
  val h = dsha256(msg)
  val sig = "3006020104020104"
  val (r, s) = decodeDERSig(sig)
  val recovered = recoverPubKeyPoints(r, s, h)
  assert(recovered.size == 4)
  recovered.flatten.map{pk =>  
    assert(pk.verify(h, r, s))
  }
}
object TestKeyRecovery4 {
  println("Test 4: [use test vectors to validate every value (sig, address, pubkey)]")
  isMainNet = false
  // tv = "test vector"
  val tv1 = """ 
    message       = 18426974636F696E205369676E6564204D6573736167653A0A17736F206D616E7920746F2063686F6F73652066726F6D21
    sighash       = BC2BE447AB153822FB7735D699E97FB60D123CF48839EA4E01D0A27A8851E497
    signed hash   = sha256(sighash) = 07B89120042681958748D7695C21FBCF1819E18235E258222EFF35E7A3FFA80F
    DER signature = 3006020107020104
    
    mxFhqDzktEtt8wJX9H5XudWioZh7AcV5zh HQAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAHAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAQ= "so many to choose from!"
    # 04C0F30B1DE11DFAFA42340B60C1D486F6D88CDE3054AA0CA1235D95115C21859CC591D66B09243438B852BF9C4A2E70ED7EEA6C4235919CCC5BF34BBB84A94222
    muJWi9mxRzxa2cqv2DckjVe7HM8tvUxtyt HgAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAHAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAQ= "so many to choose from!"
    # 04095590F3231ED86265F20FCD06C215D8D2C1BFAD47EBF46285329E202C3D954E7B8E6C386B3A1F3F80B7A7D108CC88AAC44A5AD92ED2A632492BF3CDC783C3AF
    mwKw25hv1z5wmJs9HZJpGrHUiQTse7Y8XY IQAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAHAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAQ= "so many to choose from!"
    # 02C0F30B1DE11DFAFA42340B60C1D486F6D88CDE3054AA0CA1235D95115C21859C
    mfiCy3urQBY1SYZnVrWiK7wZHQeySu6yN1 IgAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAHAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAQ= "so many to choose from!"
    # 03095590F3231ED86265F20FCD06C215D8D2C1BFAD47EBF46285329E202C3D954E"""
    
  val tv2 = """
    message       = 18426974636F696E205369676E6564204D6573736167653A0A17736F206D616E7920746F2063686F6F73652066726F6D21
    sighash       = BC2BE447AB153822FB7735D699E97FB60D123CF48839EA4E01D0A27A8851E497
    signed hash   = sha256(sighash) = 07B89120042681958748D7695C21FBCF1819E18235E258222EFF35E7A3FFA80F
    DER signature = 3006020101020104
    
    mxCrYKsqBbFQiiQvEF2UnvYyvLTvUUC3md GwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAABAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAQ= "so many to choose from!"
    # 042213201BB85583092E2A85989370B3077DD7B8F4CFEA40F21D44FEE60B5453AFFD1CCFB9A27DB424108E0A4BFB3F0D1456B4929E59DBF51914CD7BD97C7EC94B
    mk4CLZc62HWQGZLWvxfmCwuq9oUNmeuaX3 HAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAABAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAQ= "so many to choose from!"
    # 04998964010D86A983B30B867C1A091FD0524493866F49C3B79DACC48726F3A7CA1D56D7AEBDA132BC711FC549732FADFDC0AB65B9A373BF2EE227CC2B7BDBEB9E
    muPrFsrMmoyY3kb56iDAXk9NarPHHfNUUx HwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAABAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAQ= "so many to choose from!"
    # 032213201BB85583092E2A85989370B3077DD7B8F4CFEA40F21D44FEE60B5453AF
    mybEt9mXqAsRz6dQCr1YJBcjWeeVe1bCui IAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAABAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAQ= "so many to choose from!"
    # 02998964010D86A983B30B867C1A091FD0524493866F49C3B79DACC48726F3A7CA"""
    
  val tv3 = """
    message       = 18426974636F696E205369676E6564204D6573736167653A0A17736F206D616E7920746F2063686F6F73652066726F6D21
    sighash       = BC2BE447AB153822FB7735D699E97FB60D123CF48839EA4E01D0A27A8851E497
    signed hash   = sha256(sighash) = 07B89120042681958748D7695C21FBCF1819E18235E258222EFF35E7A3FFA80F
    DER signature = 3006020104020104
    
    mnYmtKBnu4KfzmPuom4NJp7y3J88KHjcUy GwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAEAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAQ= "so many to choose from!"
    # 04767AFB64FE584EEC9452AC338CCFF57986D385219B796BAA0B26A817CB8FD7CD395C129B486C26F97653FC739F365DEEEDB4B2E633EF09629DC5EECAC5ADC18E
    n3Vemy1U4XAbJZx6ZSgCHqu6oCNUbq8gf6 HAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAEAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAQ= "so many to choose from!"
    # 0488C8A4DB81B7A71ABC3679650634BDC96FEC9F6AA32428F81868491257A79ACE0780BD8E75C994B4801051129A72428CE698607F9BD05FD18BCA892AA560F62A
    mr6oKxQ5w6VWjKgn5itEvRazRWMq9MTUS6 HQAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAEAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAQ= "so many to choose from!"
    # 04A65A76391FB958D7144B59E64D3FECF2FD5C33F6224805CD3D1C14F12EFC41D8D0308BC9714C4EE14A553E5F669128EA055A114760535AB1C9AB23E10103E7A4
    mzpFNqzKXJzisNNCdArDLA7sBqHgx52rU7 HgAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAEAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAQ= "so many to choose from!"
    # 048537DAE07F5B54821726EB5C11563054C5D88FCBD40B63399C84C4EB4BA1963815836F197A080EE54ABC7DD8BF821F2B33E8AE556774F1C64DF408BF51750433
    mwtQH8ngCqbrdf6dkAgPoC1W5okN6iP65R HwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAEAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAQ= "so many to choose from!"
    # 02767AFB64FE584EEC9452AC338CCFF57986D385219B796BAA0B26A817CB8FD7CD
    mmrvzX4YT8xecvniJqvanG54nXarSBKfUZ IAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAEAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAQ= "so many to choose from!"
    # 0288C8A4DB81B7A71ABC3679650634BDC96FEC9F6AA32428F81868491257A79ACE
    mmTHeD4ieD18pLRcqRmotfTzsDUnPG4nDF IQAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAEAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAQ= "so many to choose from!"
    # 02A65A76391FB958D7144B59E64D3FECF2FD5C33F6224805CD3D1C14F12EFC41D8
    mgc8VJrGBtEbmtjU3HUs5NMGe3gcAeemhc IgAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAEAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAQ= "so many to choose from!"
    # 038537DAE07F5B54821726EB5C11563054C5D88FCBD40B63399C84C4EB4BA19638"""
  
  Seq(tv1, tv2, tv3).map{tv =>
    val lines = tv.lines.map(_.trim).filterNot(_.isEmpty).toArray  // remove empty lines
    val msg = lines(0).drop("message       = ".size)
    val sig = lines(3).drop("DER signature = ".size)
    val (r, s) = decodeDERSig(sig)
    val h = dsha256(msg.decodeHex)
    val rpks = recoverPubKeyPoints(r, s, h).flatten // .reverse
    val pks = rpks.map(rpk => new ECCPubKey(rpk, false))
    val allRPks:Seq[ECCPubKey] = pks ++ rpks.map{rpk =>
      new ECCPubKey(rpk, true)
    }
    val testpks = lines.drop(4).grouped(2).toArray
    allRPks zip testpks map {
      case (rpk, line12) =>
        val line1 = line12(0)
        val a = line1.split(" ")
        val addr = a(0)
        val sig = a(1)
        val msg = a.drop(2).reduceLeft(_ + " " + _).drop(1).dropRight(1)
        assert(rpk.point.verify(h, r, s))          
        assert(msg == "so many to choose from!")
        val pkHex = line12(1).split("#")(1).trim
        assert(rpk.encodeRecoverySig(r, s, h).encodeBase64 == sig)
        assert(pkHex.toLowerCase == rpk.hex.toLowerCase)
        val btcPk = new PubKey_P2PKH(rpk, false) // testnet
        val newAddr = btcPk.address
        assert(newAddr == addr, s"Expected $addr, found ${newAddr}")        
    }
  }
}
object TestKeyRecovery5 {
  println("Test 5: [check that encode and decode for recovery are correct]")
  // test that decoding and encoding works correctly
  1 to 10 foreach {i =>
    1 to 10 foreach{j =>
      0 to 7 foreach {k => 
        val e = encodeRecoverySigForIndex(k, p - i, p - j)
        val (_k, _i, _j) = decodeRecoverySig(e)
        assert(_k == k)
        assert(_i == p - i)
        assert(_j == p - j)

        val int1 = BigInt(sha256Bytes2Bytes(s"$i|$j".getBytes)).abs
        val int2 = BigInt(sha256Bytes2Bytes(s"$i~$j".getBytes)).abs
        val e1 = encodeRecoverySigForIndex(k, int1, int2)
        val (_k1, _int1, _int2) = decodeRecoverySig(e1)
        assert(_k1 == k)
        assert(_int1 == int1)
        assert(_int2 == int2)
        print(".")        
      }     
    }
  } 
  println
}

object TestKeyRecovery6 {
  println("Test 6: [check that recover pubKey returns EXACTLY one public key]")
  val msg = "ABCD"
  1 to 10 foreach {i =>
    Seq(true, false).map{compr =>
      Seq(true, false).map{mainNet =>
        val int = BigInt(sha256Bytes2Bytes(BigInt(i).toByteArray))
        val key = new ECCPrvKey(int mod n, compr)
        val p2pkhKey = new PrvKey_P2PKH(key, mainNet)
        val address = p2pkhKey.pubKey.address
        val sig = key.signMessageBitcoinD(msg)

        val (byteIndex, r, s) = decodeRecoverySig(sig.decodeBase64) // byteIndex will be between 0 and 7 inclusive

        if (compr) assert(byteIndex >= 4) // for compressed it must be less than 4
        if (!compr) assert(byteIndex < 4) // for compressed it must be greater than or equal to 4

        val recoveredPoints = recoverPubKeyPoints(r, s, dsha256(getMessageToSignBitcoinD(msg)))
        val actualIndex = byteIndex % 4 // mod 4
        val valid = recoveredPoints.zipWithIndex.collect{
          case (Some(x), `actualIndex`) =>  // i will be between 0 and 3 // it does not care compressed or uncompressed
            assert(x.verifyMessageBitcoinD(msg, sig))
            val newCompr = byteIndex >= 4
            new PubKey_P2PKH(new ECCPubKey(x, newCompr), mainNet).address
        }

        assert(valid.size == 1)
        assert(valid.head == address)
        print(".")
      }
    }
  }
  println

  val mainNet = true
  1 to 20 foreach {i =>
    val int = BigInt(sha256Bytes2Bytes(BigInt(i).toByteArray))
    val key = new PrvKey_P2SH_P2WPKH(int mod n, mainNet)
    val address = key.pubKey.address
    val eccPrvKey = key.eccPrvKey
        
    val sig = eccPrvKey.signMessageBitcoinD(msg)

    val (byteIndex, r, s) = decodeRecoverySig(sig.decodeBase64) // byteIndex will be between 0 and 7 inclusive

    assert(byteIndex >= 4) // for compressed it must be less than 4

    val recoveredPoints = recoverPubKeyPoints(r, s, dsha256(getMessageToSignBitcoinD(msg)))
    val actualIndex = byteIndex % 4 // mod 4
    val valid = recoveredPoints.zipWithIndex.collect{
      case (Some(x), `actualIndex`) =>  // i will be between 0 and 3 // it does not care compressed or uncompressed
        assert(x.verifyMessageBitcoinD(msg, sig))
        val isCompressed = byteIndex >= 4
        new PubKey_P2SH_P2WPKH(x, mainNet).address
    }

    assert(valid.size == 1)
    assert(valid.head == address, s"Found ${valid.head}. Expected $address")
    print(".")
  }
  println
  
}
object TestKeyRecovery7 {
  println("Test 7: [check that bitcoind sign and verify work]")
  
  val message = "ABCD"
  1 to 20 foreach {i =>
    Seq(true, false).map{compr =>
      val int = BigInt(sha256Bytes2Bytes(BigInt(i).toByteArray))
      val key = new ECCPrvKey(int mod n, compr)
      val sig = key.signMessageBitcoinD(message)
      val recovered:ECCPubKey = recoverPubKey(sig.decodeBase64, dsha256(getMessageToSignBitcoinD(message)))
      assert(recovered == key.eccPubKey)
      
      assert(new PubKey_P2PKH(recovered, true).address == new PrvKey_P2PKH(key, true).pubKey.address)
      assert(recovered.point.verifyMessageBitcoinD(message, sig))
    }
  }
}