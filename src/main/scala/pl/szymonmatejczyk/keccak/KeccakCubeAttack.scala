package pl.szymonmatejczyk.keccak

import scala.util.Random
import scala.annotation.tailrec


object KeccakCubeAttack extends App {
  val key = Bits128(1902381L, -12381L)

  val keccak = Keccak()

  def zeros() : Array[Long] = Array.fill(5)(0L)

  implicit val random = new Random

  def genInput(key : Bits128, msg : Long) : Array[Array[Long]] = Array(
    Array(key.first, key.second, msg, 0L, 0L),
    zeros(), zeros(), zeros(), zeros()
  )

  def encode(msg : Long) : Bits128 = {
    val array = genInput(key, msg)
    val res = keccak.encode(array, 3)
    new Bits128(res(0)(0), res(0)(1))
  }

  def encode(key : Bits128, msg : Long) : Bits128 = {
    val res = keccak.encode(genInput(key, msg), 3)
    new Bits128(res(0)(0), res(0)(1))
  }

  def randomMsg : Generator[Set[Int]] = for {
    listOfOnes <- sets[Int](17, choose(0, 64))
  } yield listOfOnes

  type Cube = List[Int]

  type Superpoly = List[Int]

  def sumCube(key : Bits128, cube : Cube) : Bits128 = {
    cube.toSet.subsets
      .map((subset : Set[Int]) => onesOnPositions(subset.toList))
      .foldLeft(Bits128.zero)(_ ^ encode(key, _))
  }

  def polyCube(cube : Cube)(key : Bits128) : Bits128 = sumCube(key, cube)

  def findSuperpolyFactors(cube : Cube) : List[(Int, Bits128)] = {
    val activeKeyBits = (0 to 127).toArray
      .map(Bits128.onePositive)
      .map(key => sumCube(key, cube))
      .zipWithIndex.map(x => x.swap)

    activeKeyBits.toList
  }

  def isLinear(cube : Cube, outputBitPos : Int) : Boolean = {
    val poly = polyCube(cube) _
    (1 to 10) forall {
      _ =>
        val (x, y) = (Bits128.random, Bits128.random)
        (poly(Bits128.zero) ^ poly(x) ^ poly(y)).getBit(outputBitPos) == poly(x ^ y)(outputBitPos)
    }
  }

  def isConstant(cube : Cube, outputBitPos : Int) : Boolean = {
    val poly = polyCube(cube) _
    val results = (1 to 10).map (_ => poly(Bits128.random)(outputBitPos)).toSet
    results.size == 1
  }

  def addRandomIndex(cube : Cube) : Cube = {
    oneOf[Int]((0 to 63).filter(x => !cube.contains(x)):_*).generate +: cube
  }

  @tailrec
  def findMaxterm(cube : Cube, outputBitPos : Int) : Cube = {
    if (isConstant(cube, outputBitPos))
      findMaxterm(cube.tail, outputBitPos)
    else
      if (!isLinear(cube, outputBitPos))
        findMaxterm(addRandomIndex(cube), outputBitPos)
      else
        cube
  }

  for (_ <- 0 to 50) {
    val cubeBits = randomMsg.generate
    var cube = cubeBits.toList.sorted
    cube = findMaxterm(cube, 0)
    println("Maxterm cube:\t" + cube.mkString(";") + s"(${cube.size})")
    val constants = sumCube(Bits128.zero, cube)
    println("Consts\t" + constants.toString)
    val superpolyFactors = findSuperpolyFactors(cube)
    val superpolyNonzero = superpolyFactors
      .filter(x => x._2 != constants)
    if (superpolyNonzero.nonEmpty) {
      println("Active Key Bits:")
      superpolyNonzero.map(
        x => s"${x._1}\t" + x._2.toString()
      ).foreach(println)
    }
  }
}
