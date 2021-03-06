package fpinscala.parallelism

import java.util.concurrent._
import language.implicitConversions

object Par {
  type Par[A] = ExecutorService => Future[A]


  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a) // `unit` is represented as a function that returns a `UnitFuture`, which is a simple implementation of `Future` that just wraps a constant value. It doesn't use the `ExecutorService` at all. It's always done and can't be cancelled. Its `get` method simply returns the value that we gave it.

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true

    def get(timeout: Long, units: TimeUnit): A = get

    def isCancelled = false

    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = // `map2` doesn't evaluate the call to `f` in a separate logical thread, in accord with our design choice of having `fork` be the sole function in the API for controlling parallelism. We can always do `fork(map2(a,b)(f))` if we want the evaluation of `f` to occur in a separate thread.
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      UnitFuture(f(af.get, bf.get)) // This implementation of `map2` does _not_ respect timeouts, and eagerly waits for the returned futures. This means that even if you have passed in "forked" arguments, using this map2 on them will make them wait. It simply passes the `ExecutorService` on to both `Par` values, waits for the results of the Futures `af` and `bf`, applies `f` to them, and wraps them in a `UnitFuture`. In order to respect timeouts, we'd need a new `Future` implementation that records the amount of time spent evaluating `af`, then subtracts that time from the available time allocated for evaluating `bf`.
    }

  def fixedMap2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      new Future[C] {
        override def cancel(mayInterruptIfRunning: Boolean): Boolean =
          af.cancel(mayInterruptIfRunning) && bf.cancel(mayInterruptIfRunning)

        override def isCancelled: Boolean = af.isCancelled || bf.isCancelled

        override def isDone: Boolean = af.isDone && bf.isDone

        override def get(): C = f(af.get, bf.get)

        override def get(timeout: Long, unit: TimeUnit): C = {
          val startTime = System.currentTimeMillis()
          val a = af.get(timeout, unit)
          val elapsedTime = System.currentTimeMillis() - startTime
          val remainingTime = unit.toMillis(timeout) - elapsedTime
          val b = bf.get(remainingTime, TimeUnit.MILLISECONDS)
          f(a, b)
        }
      }
    }

  def asyncF[A, B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    val fbs: List[Par[B]] = ps.map(asyncF(f))
    sequence(fbs)
  }

  def parFilter[A](ps: List[A])(f: A => Boolean): Par[List[A]] = {
    val fbs: List[Par[List[A]]] = ps.map(asyncF(x => if (f(x)) List(x) else List()))
    val p: Par[List[List[A]]] = sequence(fbs)
    map(p)(x => x.flatten)
  }

  def sequence[A](ps: List[Par[A]]): Par[List[A]] = {
    ps.foldRight(Par.unit(Nil: List[A])) {
      case (pa, acc) => map2(pa, acc)(_ :: _)
    }
  }

  def fork[A](a: => Par[A]): Par[A] = // This is the simplest and most natural implementation of `fork`, but there are some problems with it--for one, the outer `Callable` will block waiting for the "inner" task to complete. Since this blocking occupies a thread in our thread pool, or whatever resource backs the `ExecutorService`, this implies that we're losing out on some potential parallelism. Essentially, we're using two threads when one should suffice. This is a symptom of a more serious problem with the implementation, and we will discuss this later in the chapter.
    es => es.submit(new Callable[A] {
      def call = a(es).get
    })

  def map[A, B](pa: Par[A])(f: A => B): Par[B] =
    map2(pa, unit(()))((a, _) => f(a))

  def sortPar(parList: Par[List[Int]]) = map(parList)(_.sorted)

  def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean =
    p(e).get == p2(e).get

  def delay[A](fa: => Par[A]): Par[A] =
    es => fa(es)

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    es =>
      if (run(es)(cond).get) t(es) // Notice we are blocking on the result of `cond`.
      else f(es)

  /* Gives us infix syntax for `Par`. */
  implicit def toParOps[A](p: Par[A]): ParOps[A] = new ParOps(p)

  class ParOps[A](p: Par[A]) {


  }

}

object Examples extends App {

  import Par._

  def sum(ints: IndexedSeq[Int]): Int = // `IndexedSeq` is a superclass of random-access sequences like `Vector` in the standard library. Unlike lists, these sequences provide an efficient `splitAt` method for dividing them into two parts at a particular index.
    if (ints.size <= 1)
      ints.headOption getOrElse 0 // `headOption` is a method defined on all collections in Scala. We saw this function in chapter 3.
    else {
      val (l, r) = ints.splitAt(ints.length / 2) // Divide the sequence in half using the `splitAt` function.
      sum(l) + sum(r) // Recursively sum both halves and add the results together.
    }

  def sumPar(ints: IndexedSeq[Int]): Par[Int] = {
    if (ints.length <= 1)
      Par.unit(ints.headOption.getOrElse(0))
    else {
      val (l, r) = ints.splitAt(ints.length / 2) // Divide the sequence in half using the `splitAt` function.
      Par.map2(Par.fork(sumPar(l)), Par.fork(sumPar(r)))(_ + _)
    }
  }

  def divideAndConquer[A, B](vec: IndexedSeq[A])(f: (A, A) => A, defaultValue: A): Par[A] = fork {
    if (vec.size <= 1)
      Par.unit(vec.headOption getOrElse defaultValue)
    else {
      val (l, r) = vec.splitAt(vec.length / 2)
      Par.map2(
        divideAndConquer(l)(f, defaultValue),
        divideAndConquer(r)(f, defaultValue)
      )(f)
    }
  }

  def rewrittenSum(ints: IndexedSeq[Int]): Par[Int] = divideAndConquer(ints)(_ + _, 0)

  def parallelMax(ints: IndexedSeq[Int]): Par[Int] = divideAndConquer(ints)((x, y) => Seq(x, y).max, ints.head)

  // P110
  def wordCount(paragraphs: List[String]): Par[Int] = {

    def aux[A, B](vec: IndexedSeq[A])(f: A => B, g: (B, B) => B, defaultValue: B): Par[B] = fork {
      if (vec.size <= 1)
        Par.unit(vec.headOption.map(f) getOrElse defaultValue)
      else {
        val (l, r) = vec.splitAt(vec.length / 2)
        Par.map2(
          aux(l)(f, g, defaultValue),
          aux(r)(f, g, defaultValue)
        )(g)
      }
    }

    aux(paragraphs.toIndexedSeq)(s => s.split(" ").length, _ + _, 0)
  }

  def map3[A, B, C, D](a: Par[A], b: Par[B], c: Par[C])(f: (A, B, C) => D): Par[D] = {
    val value = map2(a, b)((a, b) => f.curried.apply(a)(b))
    map2(c, value)((x, y) => y(x))
  }

  def map4[A, B, C, D, E](a: Par[A], b: Par[B], c: Par[C], d: Par[D])(f: (A, B, C, D) => E): Par[E] = {
    val value = map3(a, b, c)((a, b, c) => f.curried.apply(a)(b)(c))
    map2(d, value)((x, y) => y(x))
  }

  def map5[A, B, C, D, E, F](a: Par[A], b: Par[B], c: Par[C], d: Par[D], e: Par[E])(f: (A, B, C, D, E) => F): Par[F] = {
    val value = map4(a, b, c, d)((a, b, c, d) => f.curried.apply(a)(b)(c)(d))
    map2(e, value)((x, y) => y(x))
  }


  val es: ExecutorService = Executors.newFixedThreadPool(200)

  val seq = Vector(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)

  // This deadlocks because there are not enough threads to perform the computation
  val sum1 = Par.run(es)(rewrittenSum(seq)).get()
  println(sum1)
  val sum2 = Par.run(es)(sumPar(seq)).get()
  println(sum2)

  // p110 maximum
  val maxi = Par.run(es)(parallelMax(seq)).get()
  println(maxi)

  val paragraphs: List[String] = Seq("hello world", "i love cheese", "space rocks").toList
  val count = Par.run(es)(wordCount(paragraphs)).get()
  println(count)

  val map5SumPar = map5(unit(1), unit(1), unit(1), unit(1), unit(1))(_ + _ + _ + _ + _)
  val map5Sum = Par.run(es)(map5SumPar).get()
  println(map5Sum)

  val maxWord = Par.run(es)(wordCount(paragraphs)).get()
  println(count)

  es.shutdown()
}
