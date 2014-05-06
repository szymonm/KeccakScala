package pl.szymonmatejczyk.keccak

import scala.util.Random

case class Bits128(first : Long, second : Long) {
  def getBit(pos : Int) : Boolean = {
    if (pos < 64)
      getBitFromLong(first, pos)
    else
      getBitFromLong(second, pos - 64)
  }

  def apply(pos : Int) = getBit(pos)

  override def toString : String = {
    align(16, first.toHexString) + " " + align(16, second.toHexString)
  }

  def ^(that: Bits128) : Bits128 = {
    new Bits128(first ^ that.first, second ^ that.second)
  }

  def getOnes() : Seq[Int] = {
    (0 to 63).filter(getBitFromLong(first, _)) ++ (0 to 63).filter(getBitFromLong(second, _)).map(_ + 64)
  }
}

object Bits128 {
  def zero = Bits128(0L, 0L)
  def random(implicit rng : Random) = Bits128(rng.nextLong(), rng.nextLong())
  def onePositive(pos : Int) : Bits128 = pos match {
    case keyBit if keyBit < 64 => Bits128(unitVector(keyBit), 0L)
    case keyBit => Bits128(0L, unitVector(keyBit - 64))
  }
}
