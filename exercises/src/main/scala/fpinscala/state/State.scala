package fpinscala.state


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {

  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (a, rng2) = rng.nextInt
    if (a >= 0) (a, rng2) else nonNegativeInt(rng2)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (i, rng2) = nonNegativeInt(rng)
    (i.toDouble / (Int.MaxValue.toDouble + 1), rng2)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, rn1) = rng.nextInt
    val (d, rn2) = RNG.double(rn1)
    ((i, d), rn2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((i, d), rn) = RNG.intDouble(rng)
    ((d, i), rn)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, rn1) = RNG.double(rng)
    val (d2, rn2) = RNG.double(rn1)
    val (d3, rn3) = RNG.double(rn1)
    ((d1, d2, d3), rn3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    def aux(c: Int)(rng: RNG)(acc: List[Int]): (List[Int], RNG) = {
      if (c == 0)
        (acc, rng)
      else {
        val (i, r2) = rng.nextInt
        aux(c - 1)(r2)(i :: acc)
      }
    }

    val (l, rn) = aux(count)(rng)(Nil)
    (l.reverse, rn)
  }

  val doubleBetter: Rand[Double] = map(int)(x => x / (Int.MaxValue.toDouble + 1))

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = ???

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = ???

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = ???
}

case class State[S, +A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    ???

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    ???

  def flatMap[B](f: A => State[S, B]): State[S, B] =
    ???
}

sealed trait Input

case object Coin extends Input

case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
}

object StateApp extends App {
//  var rng: RNG = RNG.Simple(42)
//  println(RNG.ints(9)(rng))
//  println(RNG.ints(10)(rng))
//  println(RNG.ints(11)(rng))
//  println(RNG.ints(12)(rng))

  var rng: RNG = RNG.Simple(42)

  println(RNG.doubleBetter(rng))

  // TODO : try to combine Rand types.
}