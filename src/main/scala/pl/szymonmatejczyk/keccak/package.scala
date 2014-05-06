package pl.szymonmatejczyk

import scala.util.Random

/**
 * Created by szymonmatejczyk on 31.03.2014.
 */
package object keccak {
//  def longToBits(l : Long) : Array[Boolean] = {
//    Array.tabulate(64)(n => getBitFromLong(l, n))
//  }


//  def bitsToLong(a : Array[Boolean]) : Long = {
//    require(a.length == 64)
//    a.zipWithIndex.filter(_._1).map(x => unitVector(x._2)).foldLeft(0L)(_ | _)
//  }

  def align(n : Int, s : String) : String = "0" * (n - s.size) + s

  def unitVector(onePosition : Int) : Long = {
    require(0 <= onePosition, s"onePosition($onePosition) not lgt 0")
    require(onePosition <= 63, "onePosition > 63")
    1L << (63 - onePosition)
  }

  def onesOnPositions(ones : List[Int]) : Long = {
    ones.map(x => unitVector(x)).foldLeft(0L)(_ | _)
  }

  def getBitFromLong(l : Long, pos : Int) : Boolean = {
    (l & unitVector(pos)) != 0
  }

  trait Generator[+T] {
    self =>
    def generate : T

    def map[S](f : T => S) : Generator[S] = new Generator[S] {
      def generate = f(self.generate)
    }

    def flatMap[S](f : T => Generator[S]) : Generator[S] = new Generator[S] {
      def generate = f(self.generate).generate
    }
  }

  lazy val rng = new Random
  val integers = new Generator[Int] {
    def generate = rng.nextInt()
  }

  val posIntegers = for (i <- integers) yield if (i >= 0) i else -i

  val booleans = for (i <- integers) yield i > 0

  def choose(lo: Int, hi: Int) : Generator[Int] = for (x <- posIntegers) yield (lo + x) % (hi - lo)

  def oneOf[T](xs : T*) : Generator[T] = for (idx <- choose(0, xs.length)) yield xs(idx)

  def single[T](x : T) = new Generator[T] {
    def generate = x
  }

  def emptyLists[T] : Generator[List[T]] = single(Nil)

  def lists[T](generator : Generator[T]) : Generator[List[T]] = for {
    isEmpty <- booleans
    list <- if (isEmpty) emptyLists[T] else nonEmptyLists[T](generator)
  } yield list

  def nonEmptyLists[T](generator : Generator[T]) = for {
    head <- generator
    tail <- lists[T](generator)
  } yield head :: tail

  def sets[T](n : Int, generator : Generator[T]) = new Generator[Set[T]] {
    def generate = (1 to n).map(_ => generator.generate).toSet
  }
}
