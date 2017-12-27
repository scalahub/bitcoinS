
package sh

import sh.ecc.PrvKey
import sh.ecc.Util._
import sh.btc.BitcoinUtil._

object TestCompressedPubKey extends App {
  /*  https://bitcoin.stackexchange.com/a/22881/2075 */
  val str = """
Private as int 79186670301299046436858412936420417076660923359050732094116068951337164773779
Public address 1EE8rpFCSSaBmG19sLdgQLEWuDaiYVFT9J
Message: test123
Signature: IGP5aqo6+oKzUF4/Pq6dfksKxlcy1CehKv6aKA6GUMGOf9Jd62UWRMNwN3KXnOHi0PdBekVBlr23EKa7sGDgTtI=

Private as int 104418396969965692461517188368945301704108638856440619106351414329473623554759
Public address 18m1EvVCeFsupQSsdZ3rTPGjL6j9hQpoj5
Message: test123
Signature: H5TIq/7pLqy2M9lUGUa9i1kxML4EmWN+MwiyijquyMhcbTuAiOjffxRz7xayj4qaAig8Y1OZFn/aqWDkM5GXVqQ=

Private as int 26841850108733854068844877092845755783895918659584173218404826715393242032133
Public address 1FRUjbVX9bbctP6y7Z7jmBufdW6KyoSvGw
Message: test123
Signature: HxypbxPc+0rICT/x6AWkGoQjE/rE7UtCLy0eS0uGn4WFGlrLkJxJDgJcIgwGDH6MdIiWL/E+cvPv2dqeHw5zCwA=

Private as int 86520705923672484424543748367654505450021158466896423122495789612346391207140
Public address 1NQvJ2X1KVwuYPQRdeiDbFanLod72UPEjg
Message: test123
Signature: IAD0xd4DofuKthgkVUfH8JJF/zeKeGL9b4gdYCb6PxhVcHd4KiX7lFAN2Kun9wUC21T0Z0bPnrsTlAIHWN03P20=

Private as int 49271812223912890183372000738074590620901220493616955749035660411043423358313
Public address 1EAGUmQG2evn1ujibn1meucRt5URGr7TKS
Message: test123
Signature: H1z4EjRWIcxYLu82qXDKVwvX74EGr5Zc8hnkL8NEO9fRAGOALDrFakMTq+gVoF+qf+8jgZSI8Lomvi1sSx8Zy3Y=

Private as int 4777466271014663949802819381765970429152817494201827532502916324303349007402
Public address 1Ea6tSV9wrofLkfqepnbLuwDKSMgUNd8k8
Message: test123
Signature: IKJKgEi+x77OgaW0fr884GZrSh+/0cuP+TpY7EUm00EMXODggkAxLPSGfXSNWEjP5jNcSGJjjsxO1SqnNxMnowA=

Private as int 30773778924833153278326239976267448371250861545391810261071816760957004408309
Public address 13a8JzTy6igoA7QfQyZVsDCpQvXY4eBj9i
Message: test123
Signature: H4jdYEux4k1zQaXby1OXSfdi6I7XgdkngA1F/E1rfcSHANqDQxCb3cAxnVG88p8jxyoJKPZh8y/XdYLwPhgmEMM=

Private as int 61103943668025454604648533394738966390945260249802700793400423519535802730901
Public address 19AnGbm4ZxhcUQ18Ttcpxd5AjQTtk7TfUy
Message: test123
Signature: ICLRZEqnmnOe21ZmOiKKxIpgN3F8gyxkHRYuXVJucXQAYv2FSwhrBU7djsZcDx0hSy5ilsRMBzBBIsuqkxQBEb4=

Private as int 73831678241472856909648189538766342154493645679368128703486299335910825024625
Public address 1MduqSPRnVWjYKNw9XyxDrTd4ScDGeB9h7
Message: test123
Signature: IADsZCQ4jSdVXJXFLMVmdvTL9Tj/P/ajJxOfbcsDUkqPANIxZZBd0aY4dqbglYBWzltXfGgVzRb6QSlDTps9bc0=

Private as int 39596642972382804109878550299664212933475275372831393917296101877715292328542
Public address 1KTMQ9PR7BTV7wofHW74nekbsDi4GYAEzN
Message: test123
Signature: IC7iMUeYPL9SuwIfs2BDoUO3cuMUVFxegg50A5HkKLeAAHyXhyVmnwgH/EzkCpITKkkn5PTAoZ6ZZv8/4PR6bAA=

Private as int 27286465058545275041328003731375053608759800669744477817022805727071484951337
Public address 1KoZDczy6zyaRgHGVGC7pDYHVfsVo4Zo29
Message: test123
Signature: IADffy/L1F2PVhegdf0t4uFghkMH27nBSgOqK0Ikks+lUP33OvEckmcN1g1qfYRhE713I/KRT3DFJ1pP4xzwuQA=

Private as int 108826293911840673209374265929265828788892543057636454753466906283594659795938
Public address 1Ctimb69eWuR7treaygbPcASkoqUwPGPn1
Message: test123
Signature: ILFG8FtncxYlOIC3ts7r3YI86OWT2TMD108MAokN++YjUaUPLxi1hwa+4BYf6zJ/NYIuGWG+sfK7zv8GzvONAAA=

Private as int 35662537539778787177688141198176047348648692780898797308846230271410478493419
Public address 1CiLSHH4CXzgPkmbazysvEX8ykb8vU2feB
Message: test123
Signature: H3UW7wX01OMT7njU/f/PQtD+hCNPoIy0rALf0PEnZQYAAJYuisnyAVkVIC7qpGcSugPKnf8LHdCQLjtt60gczRY=

Private as int 23984934282846201135004307705980641080962872643161302353166056218330425914143
Public address 1BcN5pYGUUChE3dk5CCiQeULESZzFYGHPC
Message: test123
Signature: IOoKQ4fmynIUV5RxzPlgU3TZr6hyyH9wXqjqU1OFagAARsDeWu8rbvlpk8MQrs+ad33BFRl3hTU03UYHtm3Kefs=

Private as int 19667464441218362309299491158177437155557792575544103707902999231594717797844
Public address 19cjvNftNKJBMcnuzHkcYrRQarjN7JMzkP
Message: test123
Signature: HwAAXzUsb6h+f1uwtl34AY6mZAhuUtzUhZwA85CjmoPfHs6AsOwzCCC3/1wjDws2JaUD0wesu1CwN9e/sFm/HmM=

Private as int 77359564092138606367423909782286964438584967790833203478204963256314910737690
Public address 1FAzeMcAsH16Vrib9AfRT569442nAxAyWs
Message: test123
Signature: ID/UoYIc6He5C1/UOEhNPj/N4r2OIqP2Zdku0ZtXv4MecyHvpOanKO2Jjy2gO+lMSHL8DJuj/WQhzHg9g3VqAAA=
"""
  val testStrs = str.lines.map(_.trim).filterNot(_.isEmpty).grouped(4)
  isMainNet = true
  testStrs.foreach{seq =>
    println
    val _prvKeyInt = seq(0).drop("Private as int ".size).trim
    val _addr = seq(1).drop("Public address ".size).trim
    val _msg = seq(2).drop("Message: ".size).trim
    val _sig = seq(3).drop("Signature: ".size).trim.decodeBase64
    
    val prvKey = new PrvKey(BigInt(_prvKeyInt), true)
    val hash = dsha256(getMessageToSignBitcoinD(_msg))
    require(prvKey.pubKey.getAddress == _addr)
    val recoveredPubKey = recoverPubKey(_sig, hash)
    require(recoveredPubKey == prvKey.pubKey)
    
    val (id, _r, _s) = decodeRecoverySig(_sig)
    require(recoveredPubKey.verify(hash, _r, _s))
    
    val our = prvKey.signMessageBitcoinD(_msg)
    val (ourId, r, s) = decodeRecoverySig(our.decodeBase64)
    assert(prvKey.pubKey.verifyMessageBitcoinD(_msg, our))
    println(" PrvKey "+_prvKeyInt)
    println("Address "+_addr)
    println("Message "+_msg)
    println("TestSig "+_sig.encodeBase64)
    println("  MySig "+our)
  }
  println("All tests passed!")
}
