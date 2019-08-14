package fpinscala.state

import fpinscala.state.State.update

import scala.collection.immutable


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

  val doubleBetter: Rand[Double] = map(nonNegativeInt)(x => x / (Int.MaxValue.toDouble + 1))

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, rnga) = ra(rng)
      val (b, rngb) = rb(rnga)
      (f(a, b), rngb)
    }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    map2(ra, rb)((_, _))

  val randIntDouble: Rand[(Int, Double)] =
    both(int, double)

  val randDoubleInt: Rand[(Double, Int)] =
    both(double, int)

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    fs.foldRight(unit(Nil: List[A])) {
      (rand, randList) => map2(rand, randList)(_ :: _)
    }
  }

  def intsWithSequence(i: Int): Rand[List[Int]] = {
    sequence(List.fill(i)(int))
  }

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {
    // f : rng => 'a, rng
    // g : 'a =>  (rng => ('b, rng))
    // flatmap() : rng => 'b, rng
    // TODO: finish this
    rng =>
      val (a, rng1) = f(rng)
      val (b, rng2) = g(a)(rng1)
      (b, rng2)

  }

  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt)(i => {
      val mod = i % n
      val alpha = (Int.MaxValue / n) * n
      if (i > alpha)
        nonNegativeLessThan(n)
      else
        unit(mod)
    })

  def mapViaFlatMap[A, B](s: Rand[A])(f: A => B): Rand[B] = {
    flatMap(s)(a => unit(f(a)))
  }

  def map2ViaFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    flatMap(ra)(a => map(rb)(b => f(a, b)))
  }

  def rollDie: Rand[Int] = nonNegativeLessThan(6)
}

case class State[S, +A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    State(s => {
      val (a, s2) = run(s)
      (f(a), s2)
    })

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    State(s => {
      val (a, s2) = run(s)
      val (b, s3) = sb.run(s2)
      (f(a, b), s3)
    })

  def flatMap[B](f: A => State[S, B]): State[S, B] = {
    State(s => {
      val (a, s2) = run(s)
      f(a).run(s2)
    })
  }
}

sealed trait Input

case object Coin extends Input

case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]


  def update(input: Input)(machine: Machine): Machine =
    (machine, input) match {
      case (Machine(true, candies, coins), Coin) => Machine(locked = false, candies, coins + 1)
      case (Machine(false, candies, coins), Turn) => Machine(locked = true, candies - 1, coins)
      case (m, _) => m
    }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {
    for {
      _ <- State.sequence(inputs.map(x => modify(update(x))))
      machine <- get
    } yield (machine.candies, machine.coins)
  }

  def unit[S, A](a: A): State[S, A] = State(s => (a, s))

  def sequence[S, A](l: List[State[S, A]]): State[S, List[A]] = {
    l.foldRight(State.unit[S, List[A]](Nil)) {
      (state, accumulator) => {
        state.map2(accumulator)(_ :: _)
      }
    }
  }

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()
}

object StateApp extends App {
  //  var rng: RNG = RNG.Simple(42)
  //  println(RNG.ints(9)(rng))
  //  println(RNG.ints(10)(rng))
  //  println(RNG.ints(11)(rng))
  //  println(RNG.ints(12)(rng))

  var rng: RNG = RNG.Simple(6464)
  //
  //  println(RNG.doubleBetter(rng))
  //  println(RNG.randIntDouble(rng))
  //
  //  private val value = List.fill(9)(RNG.int)
  //
  //  val seq = RNG.sequence(value)(rng)
  //  println(seq)
  //  println(RNG.intsWithSequence(9)(rng))
  println(RNG.nonNegativeLessThan(1000)(rng))
  val zero = RNG.rollDie(RNG.Simple(5))._1
  println(zero)

  val x = State.simulateMachine(Seq(Coin, Turn, Coin, Turn, Turn, Turn, Coin, Turn, Coin, Coin).toList).run(Machine(true, 5, 10))
  println(x)

}