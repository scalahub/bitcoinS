package sh

import sh.ecc._

object TestVectorsRFC6979 {
  //Haskoin test vectors for RFC 6979 ECDSA (secp256k1, SHA-256)"
  /*
   * https://bitcointalk.org/index.php?topic=285142.msg3300992#msg3300992
  "Haskoin test vectors for RFC 6979 ECDSA (secp256k1, SHA-256)"
  "(PrvKey HEX, PrvKey WIF, message, R || S as HEX, sig as DER)"
  ( "0000000000000000000000000000000000000000000000000000000000000001"
  , "KwDiBf89QgGbjEhKnhXJuH7LrciVrZi3qYjgd9M7rFU73sVHnoWn"
  , "Everything should be made as simple as possible, but not simpler."
  , "33a69cd2065432a30f3d1ce4eb0d59b8ab58c74f27c41a7fdb5696ad4e6108c96f807982866f785d3f6418d24163ddae117b7db4d5fdf0071de069fa54342262"
  , "3044022033a69cd2065432a30f3d1ce4eb0d59b8ab58c74f27c41a7fdb5696ad4e6108c902206f807982866f785d3f6418d24163ddae117b7db4d5fdf0071de069fa54342262"
  )
  ( "fffffffffffffffffffffffffffffffebaaedce6af48a03bbfd25e8cd0364140"
  , "L5oLkpV3aqBjhki6LmvChTCV6odsp4SXM6FfU2Gppt5kFLaHLuZ9"
  , "Equations are more important to me, because politics is for the present, but an equation is something for eternity."
  , "54c4a33c6423d689378f160a7ff8b61330444abb58fb470f96ea16d99d4a2fed07082304410efa6b2943111b6a4e0aaa7b7db55a07e9861d1fb3cb1f421044a5"
  , "3044022054c4a33c6423d689378f160a7ff8b61330444abb58fb470f96ea16d99d4a2fed022007082304410efa6b2943111b6a4e0aaa7b7db55a07e9861d1fb3cb1f421044a5"
  )
  ( "fffffffffffffffffffffffffffffffebaaedce6af48a03bbfd25e8cd0364140"
  , "L5oLkpV3aqBjhki6LmvChTCV6odsp4SXM6FfU2Gppt5kFLaHLuZ9"
  , "Not only is the Universe stranger than we think, it is stranger than we can think."
  , "ff466a9f1b7b273e2f4c3ffe032eb2e814121ed18ef84665d0f515360dab3dd06fc95f5132e5ecfdc8e5e6e616cc77151455d46ed48f5589b7db7771a332b283"
  , "3045022100ff466a9f1b7b273e2f4c3ffe032eb2e814121ed18ef84665d0f515360dab3dd002206fc95f5132e5ecfdc8e5e6e616cc77151455d46ed48f5589b7db7771a332b283"
  )
  ( "0000000000000000000000000000000000000000000000000000000000000001"
  , "KwDiBf89QgGbjEhKnhXJuH7LrciVrZi3qYjgd9M7rFU73sVHnoWn"
  , "How wonderful that we have met with a paradox. Now we have some hope of making progress."
  , "c0dafec8251f1d5010289d210232220b03202cba34ec11fec58b3e93a85b91d375afdc06b7d6322a590955bf264e7aaa155847f614d80078a90292fe205064d3"
  , "3045022100c0dafec8251f1d5010289d210232220b03202cba34ec11fec58b3e93a85b91d3022075afdc06b7d6322a590955bf264e7aaa155847f614d80078a90292fe205064d3"
  )
  ( "69ec59eaa1f4f2e36b639716b7c30ca86d9a5375c7b38d8918bd9c0ebc80ba64"
  , "KzmcSTRmg8Gtoq8jbBCwsrvgiTKRrewQXniAHHTf7hsten8MZmBB"
  , "Computer science is no more about computers than astronomy is about telescopes."
  , "7186363571d65e084e7f02b0b77c3ec44fb1b257dee26274c38c928986fea45d0de0b38e06807e46bda1f1e293f4f6323e854c86d58abdd00c46c16441085df6"
  , "304402207186363571d65e084e7f02b0b77c3ec44fb1b257dee26274c38c928986fea45d02200de0b38e06807e46bda1f1e293f4f6323e854c86d58abdd00c46c16441085df6"
  )
  ( "00000000000000000000000000007246174ab1e92e9149c6e446fe194d072637"
  , "KwDiBf89QgGbjEhKnhXJwe1E2mCa8asowBrSKuCaBV6EsPYEAFZ8"
  , "...if you aren't, at any given time, scandalized by code you wrote five or even three years ago, you're not learning anywhere near enough"
  , "fbfe5076a15860ba8ed00e75e9bd22e05d230f02a936b653eb55b61c99dda4870e68880ebb0050fe4312b1b1eb0899e1b82da89baa5b895f612619edf34cbd37"
  , "3045022100fbfe5076a15860ba8ed00e75e9bd22e05d230f02a936b653eb55b61c99dda48702200e68880ebb0050fe4312b1b1eb0899e1b82da89baa5b895f612619edf34cbd37"
  )
  ( "000000000000000000000000000000000000000000056916d0f9b31dc9b637f3"
  , "KwDiBf89QgGbjEhKnhXJuH7LrciVrZiib5S9h4knkymNojPUVsWN"
  , "The question of whether computers can think is like the question of whether submarines can swim."
  , "cde1302d83f8dd835d89aef803c74a119f561fbaef3eb9129e45f30de86abbf906ce643f5049ee1f27890467b77a6a8e11ec4661cc38cd8badf90115fbd03cef"
  , "3045022100cde1302d83f8dd835d89aef803c74a119f561fbaef3eb9129e45f30de86abbf9022006ce643f5049ee1f27890467b77a6a8e11ec4661cc38cd8badf90115fbd03cef"
  )   */

  
  case class HTV(key:String, keyWIF:String, msg:String, sigDER:String)
  
  // HTV =  Haskoin test vector
  val htvs = 
    Seq(
      HTV("0000000000000000000000000000000000000000000000000000000000000001"
      , "KwDiBf89QgGbjEhKnhXJuH7LrciVrZi3qYjgd9M7rFU73sVHnoWn"
      , "Everything should be made as simple as possible, but not simpler."
      , "3044022033a69cd2065432a30f3d1ce4eb0d59b8ab58c74f27c41a7fdb5696ad4e6108c902206f807982866f785d3f6418d24163ddae117b7db4d5fdf0071de069fa54342262"
      ),
      HTV( "fffffffffffffffffffffffffffffffebaaedce6af48a03bbfd25e8cd0364140"
      , "L5oLkpV3aqBjhki6LmvChTCV6odsp4SXM6FfU2Gppt5kFLaHLuZ9"
      , "Equations are more important to me, because politics is for the present, but an equation is something for eternity."
      , "3044022054c4a33c6423d689378f160a7ff8b61330444abb58fb470f96ea16d99d4a2fed022007082304410efa6b2943111b6a4e0aaa7b7db55a07e9861d1fb3cb1f421044a5"
      ),
      HTV( "fffffffffffffffffffffffffffffffebaaedce6af48a03bbfd25e8cd0364140"
      , "L5oLkpV3aqBjhki6LmvChTCV6odsp4SXM6FfU2Gppt5kFLaHLuZ9"
      , "Not only is the Universe stranger than we think, it is stranger than we can think."
      , "3045022100ff466a9f1b7b273e2f4c3ffe032eb2e814121ed18ef84665d0f515360dab3dd002206fc95f5132e5ecfdc8e5e6e616cc77151455d46ed48f5589b7db7771a332b283"
      ),
      HTV( "0000000000000000000000000000000000000000000000000000000000000001"
      , "KwDiBf89QgGbjEhKnhXJuH7LrciVrZi3qYjgd9M7rFU73sVHnoWn"
      , "How wonderful that we have met with a paradox. Now we have some hope of making progress."
      , "3045022100c0dafec8251f1d5010289d210232220b03202cba34ec11fec58b3e93a85b91d3022075afdc06b7d6322a590955bf264e7aaa155847f614d80078a90292fe205064d3"
      ),
      HTV( "69ec59eaa1f4f2e36b639716b7c30ca86d9a5375c7b38d8918bd9c0ebc80ba64"
      , "KzmcSTRmg8Gtoq8jbBCwsrvgiTKRrewQXniAHHTf7hsten8MZmBB"
      , "Computer science is no more about computers than astronomy is about telescopes."
      , "304402207186363571d65e084e7f02b0b77c3ec44fb1b257dee26274c38c928986fea45d02200de0b38e06807e46bda1f1e293f4f6323e854c86d58abdd00c46c16441085df6"
      ),
      HTV( "00000000000000000000000000007246174ab1e92e9149c6e446fe194d072637"
      , "KwDiBf89QgGbjEhKnhXJwe1E2mCa8asowBrSKuCaBV6EsPYEAFZ8"
      , "...if you aren't, at any given time, scandalized by code you wrote five or even three years ago, you're not learning anywhere near enough"
      , "3045022100fbfe5076a15860ba8ed00e75e9bd22e05d230f02a936b653eb55b61c99dda48702200e68880ebb0050fe4312b1b1eb0899e1b82da89baa5b895f612619edf34cbd37"
      ),
      HTV( "000000000000000000000000000000000000000000056916d0f9b31dc9b637f3"
      , "KwDiBf89QgGbjEhKnhXJuH7LrciVrZiib5S9h4knkymNojPUVsWN"
      , "The question of whether computers can think is like the question of whether submarines can swim."
      , "3045022100cde1302d83f8dd835d89aef803c74a119f561fbaef3eb9129e45f30de86abbf9022006ce643f5049ee1f27890467b77a6a8e11ec4661cc38cd8badf90115fbd03cef"
    )
  )
  /* https://bitcointalk.org/index.php?topic=285142.msg3299061#msg3299061
   * private key, message (string), k-value, signature
   */
  case class TV(key:String, msg:String, k:String, sig:String) // note signatures are partially wrong below (doesn't use low S values)
  val tvs = Seq(
    TV("1", "Satoshi Nakamoto", "8F8A276C19F4149656B280621E358CCE24F5F52542772691EE69063B74F15D15", "934b1ea10a4b3c1757e2b0c017d0b6143ce3c9a7e6a4a49860d7a6ab210ee3d8dbbd3162d46e9f9bef7feb87c16dc13b4f6568a87f4e83f728e2443ba586675c"),
    TV("1", "All those moments will be lost in time, like tears in rain. Time to die...", "38AA22D72376B4DBC472E06C3BA403EE0A394DA63FC58D88686C611ABA98D6B3", "8600dbd41e348fe5c9465ab92d23e3db8b98b873beecd930736488696438cb6bab8019bbd8b6924cc4099fe625340ffb1eaac34bf4477daa39d0835429094520"),
    TV("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEBAAEDCE6AF48A03BBFD25E8CD0364140", "Satoshi Nakamoto", "33A19B60E25FB6F4435AF53A3D42D493644827367E6453928554F43E49AA6F90", "fd567d121db66e382991534ada77a6bd3106f0a1098c231e47993447cd6af2d094c632f14e4379fc1ea610a3df5a375152549736425ee17cebe10abbc2a2826c"),
    TV("f8b8af8ce3c7cca5e300d33939540c10d45ce001b8f252bfbc57ba0342904181", "Alan Turing", "525A82B70E67874398067543FD84C83D30C175FDC45FDEEE082FE13B1D7CFDF1", "7063ae83e7f62bbb171798131b4a0564b956930092b33b07b395615d9ec7e15ca72033e1ff5ca1ea8d0c99001cb45f0272d3be7525d3049c0d9e98dc7582b857")
  )
}

import TestVectorsRFC6979._
object TestRFC6979 extends App {
  // test vectors 1
  tvs.map(tv => new PrvKey(tv.key, true).sign(tv.msg))
  /*  Above tvs have signature wrong (s values are high in many cases, so we will only check that the ks are correct. 
      Indeed they are. Below is the output

      Deterministic => 8f8a276c19f4149656b280621e358cce24f5f52542772691ee69063b74f15d15
      Deterministic => 38aa22d72376b4dbc472e06c3ba403ee0a394da63fc58d88686c611aba98d6b3
      Deterministic => 33a19b60e25fb6f4435af53a3d42d493644827367e6453928554f43e49aa6f90
      Deterministic => 525a82b70e67874398067543fd84c83d30c175fdc45fdeee082fe13b1d7cfdf1   */
 
  // test vectors 2
  htvs.map{htv => 
    assert (new PrvKey(htv.key, true).sign(htv.msg).toLowerCase == htv.sigDER.toLowerCase)
    println("Test passed for: "+htv.msg)
  }
}
import sh.ecc.Util._
import sh.util._
object GenerateTestVectors extends App {
  val msgs = """Absence makes the heart grow fonder.
Actions speak louder than words.
All for one and one for all.
All's fair in love and war.
All work and no play makes Jack a dull boy. 
All's well that ends well.
An apple a day keeps the doctor away.
An apple never falls far from the tree.
An ounce of prevention is worth a pound of cure.
Appearances can be deceiving.
April showers bring May flowers.
Bad news travels fast.
Beauty is in the eye of the beholder.
Beauty is only skin deep.
Beggars can't be choosers.
Behind every good man is a good woman.
The best defense is a good offense.
The best laid plans of mice and men (often go astray).
Better late than never.
Better safe than sorry.
Better to have loved and lost than never to have loved at all.
Beware of Greeks bearing gifts.
The bigger thet are the harder they fall.
A bird in the hand is worth two in the bush.
Birds of a feather flock together.
Blood is thicker than water.
Boys will be boys.
Caught between a rock and a hard place.
A chain is only a strong as its weakest link.
Charity begins at home.
The chickens have come home to roost.
Cleanliness is next to godliness.
Close, but no cigar.
Cold hands, warm heart.
The cure is worse than the disease.
Curiosity killed the cat.
The customer is always right.
Damned if you do, damned if you don't.
Discretion is the better part of valor.
Do unto others as you would have them do unto you.
A dog is a man's best friend.
Don't bite off more than you can chew.
Don't bite the hand that feeds you.
Don't change horses in midstream.
Don't count your chickens before they hatch.
Don't cry over spilt milk.
Don't cut off your nose to spite your face.
Don't judge a book by its cover.
Don't lock the stable door after the horse is gone.
Don't look a gift horse in the mouth.
Don't make a mountain out of a mole-hill.
Don't put all your eggs in one basket.
Don't put the cart before the horse.
Don't rock the boat.
Don't throw the baby out with the bathwater.
Don't upset the apple cart.
Early to bed and early to rise makes a man healthy, wealthy, and wise.
The early bird catches the worm.
Easy come, easy go.
Easy does it.
The end justifies the means.
Every cloud has a silver lining.
Every dog has its day.
Every man pays his price.
The exception that proves the rule.
Experience is the best teacher.
Feed a cold, starve a fever.
Finders keepers, losers weepers.
First things first.
Fish, or cut bait.
A fool and his money are soon parted.
A friend in need is a friend indeed.
Give the devil his due.
God helps those who help themselves.
Going to hell in a handbasket.
Good fences make good neighbors.
Good men are hard to find.
Great minds think alike.
Good things come in small packages.
Good things come to he who waits.
The grass is always greener (on the other side of the fence).
Great oaks from little acorns grow.
Half a loaf is better than none.
Haste makes waste.
Here today, gone tomorrow
He who hesitates is lost.
He who laughs last, laughs best. (or 'longest')
He who lives by the sword, dies by the sword.
Hindsight is 20-20.
His bark is worse than his bite.
Honesty is the best policy.
Honey catches more flies than vinegar.
A house is not a home.
Idle minds are the devil's workshop.
If at first you don't succeed, try, try again.
If God had meant us to fly he'd have given us wings.
If it ain't broke, don't fix it.
If it's not one thing, it's another.
If the shoe fits, wear it.
If wishes were horses then beggars would ride.
If you can't lick 'em, join 'em.
If you can't stand the heat, get out of the kitchen.
If you want a thing done right, do it yourself.
Ignorance is bliss.
Immitation is the sicerest form of flattery.
In for a penny, in for a pound.
Into every life a little rain must fall.
Its better to give than to receive.
It ain't over till the fat lady sings.
It takes one to know one.
It takes two to tango.
A jack of all trades is master of none.
Keep your powder dry.
Keeping up with the Jonses.
Kill two birds with one stone.
Laugh, and the world laughs with you; (cry, and you cry alone.)
Laughter is the best medicine.
Leave well enough alone.
A leopard cannot change its spots.
Let bygones be bygones.
Let sleeping dogs lie.
Lightning never strikes twice in the same place.
Like father, like son.
A little knowledge is a dangerous thing.
Little strokes fell great oaks.
Live and learn.
Live and let live.
Look before you leap.
Love is blind.
Make hay while the sun shines.
Man does not live by bread alone.
A Man's home is his castle.
Many hands make light work.
Measure twice, cut once.
Mind your p's and q's.
Misery loves company.
A miss is as good as a mile.
Money doesn't grow on trees.
Money is the root of all evil.
Monkey see, monkey do.
The more the merrier.
Necessity is the mother of invention.
It's not over 'till it's over.
Never put off till tomorrow what you can do today. 
Never trouble trouble till trouble troubles you.
It's never too late to change.
No news is good news.
No pain, no gain. 
No rest for the weary.
No rest for the wicked.
Nothing is certain but death and taxes.
Nothing ventured, nothing gained.
Oil and water don't mix.
Old habits die hard.
Once bitten, twice shy.
One good turn deserves another.
One man's junk is another man's treasure.
One picture is worth a thousand words.
One rotten apple spoils the whole barrel.
An ounce of prevention is worth a pound of cure.
Out of sight, out of mind.
The pen is mightier than the sword.
A penny saved is a penny earned.
People who live in glass houses shouldn't throw stones.
A place for everything and everything in its place.
Possession is nine-tenths of the law.
Practice makes perfect.
Practice what you preach.
Pride comes before a fall.
The proof of the pudding is in the eating. (Nowadays mysteriously shortened to "the proof is in the pudding")
Put up or shut up.
Put your money where your mouth is.
The road to hell is paved with good intentions.
A rolling stone gathers no moss.
Rome wasn't built in a day.
Rules were meant to be broken.
Scratch my back and I'll scratch yours.
Seeing is believing.
The show must go on.
Silence is golden.
Slow and steady wins the race.
Spare the rod, spoil the child.
Speak softly and carry a big stick.
The squeaky wheel gets the grease.
Sticks and stones may break my bones, but words (sometimes 'names') will never hurt me.  
Still waters run deep.
A stitch in time saves nine.
Strike while the iron is hot
Talk is cheap.
There's a sucker born every minute.
There's more than one way to skin a cat.
There is no accounting for tastes.
There's no fool like an old fool.
There is no honor among thieves.
There's no place like home.
There's no time like the present.
They don't make things like they used to.
Time heals all wounds.
To err is human, to forgive, divine.
Too many cooks spoil the broth.
Truth is stranger than fiction.
Turnabout is fair play.
Two heads are better than one.
Two's company, three's a crowd.
Two wrongs don't make a right.
Up a creek without a paddle.
Waste not, want not.
A watched pot never boils.
The way to a man's heart is through his stomach.
Well begun is half done.
What goes around comes around.
Whatever will be, will be.
What you see is what you get.
When it rains, it pours.
When in Rome, (do as the Romans do.)
When the cat's away the mice will play.
When one door closes another door opens.
When the going gets tough, the tough get going.
Where there's a will, there's a way.
Where there's smoke, there's fire.
A woman's work is never done.
You can't have your cake and eat it too.
You can lead a horse to water, but you can't make him drink.
You can't please everyone.
You can't take it with you.
You can't teach an old dog new tricks.
You get what you pay for.
You can't win them all.
You have to break a few eggs to make an omlette.
You have to take the good  with the bad.
You win some, you lose some.""".lines.map(_.trim).toArray
  val keys1 = (1 to msgs.size).map{i =>
    new PrvKey(i, true)
  }.toArray
  val res1 = (keys1 zip msgs).map{
    case (key, msg) => (key.key, msg, key.sign(msg))
  }
  res1 foreach println
  val keys2 = (1 to msgs.size).map{i =>
    new PrvKey(Hex.encodeBytes(sha256Bytes2Bytes(BigInt(i).toBytes)), true)
  }.toArray
  val res2 = (keys2 zip msgs).map{
    case (key, msg) => (key.key, msg, key.sign(msg))
  }
  res2 foreach println
  
}

