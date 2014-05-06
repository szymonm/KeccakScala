package pl.szymonmatejczyk.keccak

import org.specs2.mutable._
/**
 * Created by szymonmatejczyk on 31.03.2014.
 */
class KeccakTest extends Specification {
  val testInput = Array.fill[Long](5, 5)(0L)
  val keccak = Keccak(5)

  def array2dequality(ar1 : Array[Array[Long]], ar2 : Array[Array[Long]]) : Boolean = {
    ar1.flatten.zip(ar2.flatten).forall{case (a, b) => a == b}
  }

  "Keccak" should {
    "compute correct results after 1st round" in {
      val res1 = keccak.round(testInput, 0)
      val expList = List(
        0x0000000000000001L, 0x0000000000000000L, 0x0000000000000000L, 0x0000000000000000L, 0x0000000000000000L,
        0x0000000000000000L, 0x0000000000000000L, 0x0000000000000000L, 0x0000000000000000L, 0x0000000000000000L,
        0x0000000000000000L, 0x0000000000000000L, 0x0000000000000000L, 0x0000000000000000L, 0x0000000000000000L,
        0x0000000000000000L, 0x0000000000000000L, 0x0000000000000000L, 0x0000000000000000L, 0x0000000000000000L,
        0x0000000000000000L, 0x0000000000000000L, 0x0000000000000000L, 0x0000000000000000L, 0x0000000000000000L)
      val expArray = Array.tabulate(5, 5){case (row, column) => expList(row * 5 + column)}

      array2dequality(res1, expArray) shouldEqual true
    }
  }

}
