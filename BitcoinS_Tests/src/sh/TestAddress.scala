
package sh

import sh.btc.BitcoinS

object TestAddress extends App {
  def assertBitcoin(address:String, isValid:Boolean) = {
    assert(BitcoinS.isValidAddress(address) == isValid, s"$address test failed. Expected $isValid")
  }
  // test vectors from https://rosettacode.org/wiki/Bitcoin/address_validation#Java
  assertBitcoin("1AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62i", true);
  assertBitcoin("1AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62j", false);
  assertBitcoin("1Q1pE5vPGEEMqRcVRMbtBK842Y6Pzo6nK9", true);
  assertBitcoin("1AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62X", false);
  assertBitcoin("1ANNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62i", false);
  assertBitcoin("1A Na15ZQXAZUgFiqJ2i7Z2DPU2J6hW62i", false);
  assertBitcoin("BZbvjr", false);
  assertBitcoin("i55j", false);
  assertBitcoin("1AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62!", false);
  assertBitcoin("1AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62iz", false);
  assertBitcoin("1AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62izz", false);
  assertBitcoin("1Q1pE5vPGEEMqRcVRMbtBK842Y6Pzo6nJ9", false);
  assertBitcoin("1AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62I", false);
    
  println("Address isValid tests passed")
}
