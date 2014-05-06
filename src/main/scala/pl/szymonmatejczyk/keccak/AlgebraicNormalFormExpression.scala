package pl.szymonmatejczyk.keccak

trait Environment[V] {
  def get(v : V) : Boolean
}

class PublicAndSecretVariablesEnvironment(public : Array[Boolean], secret : Array[Boolean])
  extends Environment[Either[Int, Int]] {
  override def get(v: Either[Int, Int]): Boolean = v match {
    case Left(i) => public(i)
    case Right(i) => secret(i)
  }
}

class Conjunction[V](variables : Seq[V]) {
  def value(implicit env : Environment[V]) = {
    variables.map(x => env.get(x)).reduce(_ & _)
  }
}

class AlgebraicNormalFormExpression[V](conjunctions : Seq[Conjunction[V]]) {
  def value(implicit env : Environment[V]) : Boolean = {
    conjunctions.map(x => x.value(env)).reduce(_ ^ _)
  }
}

object Excercise {
  type VarRef = Either[Int, Int]
  def public(i : Int) = Left(i)
  def secret(i : Int) = Right(i)
  val anf = new AlgebraicNormalFormExpression[Either[Int, Int]](Seq(
    new Conjunction(Seq(secret(2), secret(2))),
    new Conjunction(Seq(public(1), public(2), public(3), public(4), secret(0))),
    new Conjunction(Seq(public(1), public(2), public(3), secret(1))),
    new Conjunction(Seq(secret(4))),
    new Conjunction(Seq(public(0), public(1), secret(4)))
  ))

  val env = new PublicAndSecretVariablesEnvironment(Array(false, false, true, true, false),
    Array(true, true, true, false, false))

  anf.value(env)

  def booleans = List(false, true)

  val keys = Seq(Array(true, true, true, false, false), Array(true, false, false, true, true),
    Array(true, true, true, true, true))

  val xor1 = for (v0 <- booleans; v1 <- booleans; v2 <- booleans; v3 <- booleans) yield {
    val public = Array(v0, v1, v2, v3, false)
    anf.value(new PublicAndSecretVariablesEnvironment(public, keys(0)))
  }
  println(xor1.reduce(_ ^ _))

  keys.foreach {
    key =>
      println(s"Key: ${key.mkString(" ")}")
      val xorForX0 = for (v1 <- booleans; v2 <- booleans; v3 <- booleans; v4 <- booleans) yield {
        val public = Array(false, v1, v2, v3, v4)
        anf.value(new PublicAndSecretVariablesEnvironment(public, key))
      }
      println(s"x0: ${xorForX0.reduce(_ ^ _)}")


      val xorForX1 = for (v1 <- booleans; v2 <- booleans; v3 <- booleans) yield {
        val public = Array(false, v1, v2, v3, false)
        anf.value(new PublicAndSecretVariablesEnvironment(public, key))
      }
      println(s"x1: ${xorForX1.reduce(_ ^ _)}")


      val xorForX4 = for (v0 <- booleans; v1 <- booleans) yield {
        val public = Array(v0, v1, false, false, false)
        anf.value(new PublicAndSecretVariablesEnvironment(public, key))
      }
      println(s"x4: ${xorForX4.reduce(_ ^ _)}")
  }

}
