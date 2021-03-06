package pl.szymonmatejczyk.keccak

class Keccak(tableSize : Int, RC : Array[Long], rotations : Array[Array[Int]]) {
  type LLArray = Array[Array[Long]]
  type IIArray = Array[Array[Int]]

  def genArray2d() = Array.fill[Long](tableSize, tableSize)(0L)
  def genArray1d() = Array.fill(tableSize)(0L)

  def ind = 0.to(tableSize - 1)

  val LONG_SIZE = 64

  def rotation(l : Long, bit : Int) : Long = {
    (l << bit) | (l >>> (LONG_SIZE - bit))
  }

  def round(ar : LLArray, iterNo : Int) : LLArray = {
    val res = genArray2d()

    val c = genArray1d()
    ind.foreach(x => c(x) = ar(x).reduce(_ ^ _))

    val d = genArray1d()
    ind.foreach(x => d(x) = c((x + tableSize - 1) % tableSize) ^ rotation(c((x + 1) % tableSize), 1))
    for (x <- ind; y <- ind) {
      res(x)(y) = ar(x)(y) ^ d(x)
    }
//    println("After theta:")
//    print(res)

    // B[y,2*x+3*y] = rot(A[x,y], r[x,y]),                forall (x,y) in (0…4,0…4)
    val b = genArray2d()
    for (x <- ind; y <- ind) {
      b(y)((2 * x + 3 * y) % tableSize) =
        rotation(res(x)(y),
        rotations(x)(y))
    }
//    println("After rho:")
//    print(b)
//    println("After pi:")
//    print(b)

    //  A[x,y] = B[x,y] xor ((not B[x+1,y]) and B[x+2,y]), forall (x,y) in (0…4,0…4)
    for (x <- ind; y <- ind) {
      res(x)(y) = b(x)(y) ^ (~b((x + 1) % tableSize)(y) & b((x + 2) % tableSize)(y))
    }
//    println("After chi:")
//    print(res)

    // A[0,0] = A[0,0] xor RC
    res(0)(0) = res(0)(0) ^ RC(iterNo)
//    println("After iota:")
//    print(res)

    res
  }

  def encode(ar : LLArray, iterations : Int = 20) : LLArray = {
    (0 to iterations).foldLeft(ar){
      case (array, iterNo) => round(array, iterNo)
    }
  }

//  def keccak1600(ar : Array[Array[Long]]) : LLArray = {
//    1.to(24).foldLeft(ar){case (ar, iterNo) => round(ar, r, RC(iterNo))}
//  }

//  def keccak(r : Long, c : Long)(input : Array[Int]) : Array[Long] = {
//    val s = Array.fill[Long](5, 5)(0L)
//    var P = input ++ Array(0x01, 0x00)
//
//    for (x <- ind; y <- ind) { s(x)(y) }
//  }

  def transpose(a : LLArray) : LLArray = {
    val b = genArray2d()
    for (x <-ind; y<-ind) {
      b(x)(y) = a(y)(x)
    }
    b
  }

  def print(a : LLArray) {
    for (x <- ind) {
      println(transpose(a)(x).map(x => align(16, x.toHexString).toUpperCase).mkString(" "))
    }
  }

}

object Keccak {
  val RC = Array[Long](
    0x0000000000000001L,
    0x0000000000008082L,
    0x800000000000808AL,
    0x8000000080008000L,
    0x000000000000808BL,
    0x0000000080000001L,
    0x8000000080008081L,
    0x8000000000008009L,
    0x000000000000008AL,
    0x0000000000000088L,
    0x0000000080008009L,
    0x000000008000000AL,
    0x000000008000808BL,
    0x800000000000008BL,
    0x8000000000008089L,
    0x8000000000008003L,
    0x8000000000008002L,
    0x8000000000000080L,
    0x000000000000800AL,
    0x800000008000000AL,
    0x8000000080008081L,
    0x8000000000008080L,
    0x0000000080000001L,
    0x8000000080008008L
  )


  val rotations : Array[Array[Int]] = Array(
    Array(0, 36, 3, 41, 18),
    Array(1, 44, 10, 45, 2),
    Array(62, 6, 43, 15, 61),
    Array(28, 55, 25, 21, 56),
    Array(27, 20, 39, 8, 14)
  )

//  val testInput = Array.fill[Long](5, 5)(0L)
//
//  val rs = 0.to(3).foldLeft(testInput){
//    case (ar, iterNo) =>
//      println()
//      println(s"--- Round $iterNo ---")
//      println()
//      val res = new Keccak(5, RC) round(ar, iterNo)
//      //      print(res)
//      //      println()
//      res
//  }

  def apply(n : Int = 5) : Keccak = {
    new Keccak(n, RC, rotations)
  }
}