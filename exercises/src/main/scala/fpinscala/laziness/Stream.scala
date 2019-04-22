package fpinscala.laziness

import scala.collection.mutable.ListBuffer

trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def toListRec(): List[A] = this match {
    case Empty => Nil
    case Cons(h, t) =>
      h() :: t().toListRec()
  }

  def toListTailRec(): List[A] = {
    def aux(s: Stream[A], acc: List[A]): List[A] = s match {
      case Empty => acc
      case Cons(h, t) => aux(t(), h() :: acc)
    }

    aux(this, Nil).reverse
  }

  def toListFast(): List[A] = {
    val acc = ListBuffer[A]()

    def aux(s: Stream[A]): List[A] = {
      s match {
        case Empty => acc.toList
        case Cons(h, t) =>
          acc += h()
          aux(t())
      }
    }

    aux(this)
  }

  def toList: List[A] = {
    foldRight(Nil: List[A]) {
      (a, b) =>
        a :: b
    }
  }

  def take(n: Int): Stream[A] = {

    def go(s: Stream[A], n: Int): Stream[A] = {
      s match {
        case Empty => Stream.empty
        case _ if n == 0 => Stream.empty
        case Cons(h, _) if n == 1 => Stream.cons(h(), Stream.empty)
        case Cons(h, t) => Stream.cons(h(), go(t(), n - 1))
      }
    }

    go(this, n)
  }

  def drop(n: Int): Stream[A] = {

    def go(s: Stream[A], n: Int): Stream[A] = {
      s match {
        case e@Empty => e
        case e if n == 0 => e
        case Cons(_, t) => go(t(), n - 1)
      }
    }

    go(this, n)
  }

  def takeWhile(p: A => Boolean): Stream[A] = {
    this match {
      case Cons(h, t) =>
        lazy val head = h()
        if (p(head))
          Stream.cons(head, t() takeWhile p)
        else
          Stream.empty
      case _ => Stream.empty
    }
  }

  def forAll(p: A => Boolean): Boolean = foldRight(true) {
    (s, b) => p(s) && b
  }


  def takeWhileWithFoldRight(p: A => Boolean): Stream[A] = {
    val empty: Stream[A] = Stream.empty
    foldRight(empty) {
      (a, s) => if (p(a)) Stream.cons(a, s) else empty
    }
  }

  def headOption: Option[A] = foldRight(None: Option[A]) {
    (a, _) => Some(a)
  }

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def map[B](f: A => B): Stream[B] = foldRight(Stream.empty: Stream[B]) {
    (a, sb) => Stream.cons(f(a), sb)
  }

  def filter(f: A => Boolean): Stream[A] = foldRight(Stream.empty: Stream[A]) {
    (a, sb) => if (f(a)) Stream.cons(a, sb) else sb
  }

  def append[B >: A](b: => Stream[B]): Stream[B] = foldRight(b) {
    Stream.cons(_, _)
  }

  def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight(Stream.empty[B]) {
    (a, sb) => f(a).append(sb)
  }

  def mapWithUnfold[B](f: A => B): Stream[B] = {

    Stream.unfold(this) {
      case Cons(h, t) => Some(f(h()), t())
      case Empty => None
    }
  }

  def takeWithUnfold(n: Int): Stream[A] = {
    Stream.unfold((this, n)) {
      case (_, 0) => None
      case (Cons(h, t), x: Int) => Some(h(), (t(), x - 1))
      case (_, _) => None
    }
  }

  def takeWhileWithUnfold(p: A => Boolean): Stream[A] = {
    Stream.unfold(this) {
      case Cons(h, t) if p(h()) => Some(h(), t())
      case _ => None
    }
  }

  def zipWith[B, C](s: Stream[B])(f: (A, B) => C): Stream[C] = {
    Stream.unfold((this, s)) {
      case (Cons(h, t), Cons(hh, tt)) => Some(f(h(), hh()), (t(), tt()))
      case _ => None
    }
  }

  def zipAll[B](s: Stream[B]): Stream[(Option[A], Option[B])] = {
    Stream.unfold((this, s)) {
      case (Cons(h, t), Cons(hh, tt)) => Some((Some(h()), Some(hh())), (t(), tt()))
      case (Cons(h, t), Empty) => Some((Some(h()), None), (t(), Empty))
      case (Empty, Cons(hh, tt)) => Some((None, Some(hh())), (Empty, tt()))
      case _ => None
    }
  }

  def startsWith[B >: A](s: Stream[B]): Boolean =
    zipAll(s).map(x => x).takeWhile(_._2.isDefined).forAll({
      case (x1, x2) => x1 == x2
    })

  def tailsRec: Stream[Stream[A]] = {
    this match {
      case Empty => Stream.cons(Empty, Empty)
      case Cons(_, t) => Stream.cons(this, t().tailsRec)
    }
  }

  def tails: Stream[Stream[A]] = {
    Stream.unfold(this) {
      case Empty => None
      case s@Cons(_, t) => Some(s, t())
    } append Stream(Stream.empty)
  }

  def hasSubsequence[A](s: Stream[A]): Boolean =
    tails exists (_ startsWith s)

  def scanLeft[B](z: => B)(f: (A, => B) => B): Stream[B] = {
    Stream.unfold(z, this) {
      case (_, Empty) => None
      case (i, Cons(x, t)) =>
        lazy val v = f(x(), i)
        Some(v, (v, t()))
    }
  }

  def scanRight[B](z: => B)(f: (A, => B) => B): Stream[B] = {
    foldRight(z, Stream(z)) {
      case (a, (b, sb)) =>
        lazy val x = f(a, b)
        (x, Stream.cons(x, sb))
    }._2
  }
}

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  def constant[A](a: A): Stream[A] = Stream.cons(a, constant(a))

  def from(n: Int): Stream[Int] = Stream.cons(n, from(n + 1))

  def fibs: Stream[Int] = {
    def go(fn1: Int, fn2: Int): Stream[Int] = {
      cons(fn1, go(fn2, fn1 + fn2))
    }

    go(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case None => empty
      case Some((a, news)) => cons(a, unfold(news)(f))
    }

  def fibsWithUnfold: Stream[Int] = {
    unfold((0, 1)) { case (x1, x2) => Some(x1, (x2, x1 + x2)) }
  }

  def fromWithUnfold(n: Int): Stream[Int] = {
    unfold(n) { x => Some(x, x + 1) }
  }

  def constantWithFold[A](a: A): Stream[A] = {
    unfold(a) { x => Some(x, x) }
  }

  def onesWithFold: Stream[Int] = {
    unfold(1) { _ => Some(1, 42) }
  }


}


object MainStream extends App {

  def get(i: Int): Int = {
    println(i)
    i
  }

  val s: Stream[Int] =
    Cons(() => {
      get(1)
    },
      () => Cons(() => {
        get(2)
      },
        () => Cons(() => {
          get(3)
        },
          () => Cons(() => {
            get(4)
          },
            () => Cons(() => {
              get(5)
            },
              () => Cons(() => {
                get(6)
              }, () => Empty))))))

  val s2 = Empty

  //val s2 = Cons(() => 1, () => Empty)
  //val l = s.toListFast
  //println(l)

  //println(s.drop(2).toList)

  //println(s.takeWhile(x=> x <= 3).toList)
  //println(s.forAll(x => x <= 3))
  //println(s.takeWhileWithFoldRight(x=> x <= 3).toList)
  //println(s.headOption)
  //println(s2)

  //println(s.take(3).map(x=> x + 2).filter(x => x % 2 == 0).toList)
  //println(s.append(s.map(x=> x +2)).toList)
  //println(s.flatMap(x => Stream(x, x)).toList)

  //val ones: Stream[Int] = Stream.cons(1, ones)

  //println(ones.takeWhile(_ == 1))
  //println(Stream.fromWithUnfold(5).take(10).toList)

  //println(Stream.onesWithFold.mapWithUnfold(x=> x +1).take(5).toList)
  //println(Stream.onesWithFold.mapWithUnfold(x=> x +1).take(5).toList)
  //println(Stream.empty[Int].mapWithUnfold(x=> x+1).take(5).toList)

  //println(Stream.from(10).takeWhileWithUnfold(x=> x <= 12).toList)
  //println(Stream.from(10).take(10).zipAll(Stream.from(10).take(5)).toList)
  //println(Stream.from(10).take(23).startsWith(Stream.from(10).take(20)))

  //println(Stream(1,2,3) startsWith Stream(1,2))

  //println(Stream(1, 2, 3).tails.map(x=> x.toList).toList)

  println(Stream.from(10).take(100).hasSubsequence(Stream(9, 10)))

  println(Stream.from(1).take(3).scanRight(0)(_+_).toList)
}