package fpinscala.datastructures

import scala.annotation.tailrec

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = foldLeft(ints, 0)(_ + _)

  def product(ds: List[Double]): Double = foldLeft(ds, 1.0)(_ * _)

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))


  def fill[A](n: Int)(a: => A): List[A] = {
    @tailrec
    def loop(n: Int, acc: List[A]): List[A] = {
      if (n == 0) acc
      else loop(n - 1, Cons(a, acc))
    }

    loop(n, Nil)
  }


  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, xs) => xs
  }

  // We could raise an error if the list is already Nil

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => Nil
    case Cons(_, xs) => Cons(h, xs)
  }

  def drop[A](l: List[A], n: Int): List[A] =
    if (n == 0) l
    else drop(tail(l), n - 1)

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(a, as) => if (f(a)) dropWhile(as, f) else l
  }

  /*
  * The way the list data structure is implemeted makes it impossible to "jump" to the end of the list.
  * We are forced to iterate until the end in order to remove the last element.
  * */
  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, Nil) => Nil
    case Cons(x1, xs) => Cons(x1, init(xs))
  }

  def length[A](l: List[A]): Int = foldLeft(l, 0)((s, _) => s + 1)

  @tailrec
  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B =
    l match {
      case Nil => z
      case Cons(head, xs) => foldLeft(xs, f(z, head))(f)
    }

  def reverse[A](l: List[A]): List[A] = foldLeft[A, List[A]](l, Nil)((acc, h) => Cons(h, acc))

  def foldLeft2[A, B](l: List[A], z: B)(f: (B, A) => B): B = foldRight(reverse(l), z)((a, b) => f(b, a))

  def foldRight2[A, B](l: List[A], z: B)(f: (A, B) => B): B = foldLeft(reverse(l), z)((a, b) => f(b, a))

  def length2[A](l: List[A]): Int = foldRight2(l, 0)((_, s) => 1 + s)

  def appendViaFoldRight[A](a1: List[A], a2: List[A]): List[A] = {
    foldRight(a1, a2)((element, acc) => {
      Cons(element, acc)
    })
  }

  def appendViaFoldLeft[A](a1: List[A], a2: List[A]): List[A] = {
    foldLeft(reverse(a1), a2)((acc, element) => {
      Cons(element, acc)
    })
  }

  def concat[A](l: List[List[A]]): List[A] = {
    foldLeft(l, Nil: List[A])(append)
  }

  def addOne(l: List[Int]): List[Int] = {
    foldRight(l, Nil: List[Int])((h, acc) => Cons(h + 1, acc))
  }

  def ldToString(l: List[Double]): List[String] = {
    foldRight(l, Nil: List[String])((h, acc) => Cons(h.toString, acc))
  }

  def map[A, B](l: List[A])(f: A => B): List[B] =
    foldRight(l, Nil: List[B])((h, acc) => Cons(f(h), acc))

  def filter[A](l: List[A])(f: A => Boolean): List[A] =
    foldRight(l, Nil: List[A])((h, acc) => if (f(h)) Cons(h, acc) else acc)

  def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] =
    concat(map(l)(f))

  def filterViaFlatMap[A](l: List[A])(f: A => Boolean): List[A] =
    flatMap(l)(x => if (f(x)) Cons(x, Nil) else Nil)

  def sumLists(a: List[Int], b: List[Int]): List[Int] = {
    def go(_a: List[Int], _b: List[Int], acc: List[Int]): List[Int] = {
      (_a, _b) match {
        case (Cons(ah, at), Cons(bh, bt)) => go(at, bt, Cons(ah + bh, acc))
        case (Cons(ah, at), Nil) => go(at, Nil, Cons(ah, acc))
        case (Nil, Cons(bh, bt)) => go(Nil, bt, Cons(bh, acc))
        case (Nil, Nil) => acc
      }
    }

    reverse(go(a, b, Nil))
  }

  def zipWith[A, B, C](a: List[A], b: List[B])(f: (A, B) => C): List[C] = {
    def go(_a: List[A], _b: List[B], acc: List[C]): List[C] = {
      (_a, _b) match {
        case (Cons(ah, at), Cons(bh, bt)) => go(at, bt, Cons(f(ah, bh), acc))
        case (_, Nil) => acc
        case (Nil, _) => acc
      }
    }

    reverse(go(a, b, Nil))
  }

  def hasSubSequence[A](sup: List[A], sub: List[A]): Boolean = {
    sup match {
      case Nil => false
      case Cons(_,t) =>
        val matches : List[Boolean] = filter(zipWith(sup, sub)((a, b) => a == b))( x=> x)
        if (length(matches) == length(sub))
          true
        else
          hasSubSequence(t, sub)
    }
  }
}

object ListMain extends App {
  val l1 = List(1, 2, 3, 4)
  val l2 = Nil
  val l3 = List(5, 6, 7, 8)
  val l4 = List(1)
  val l5 = List(1,2,3)
  val ld = List(1.0, 2.0, 3.0)

  import List._

  //val l6 = fill(10000000)(3)


  val res = hasSubSequence(l1, l4)

  print(res)

}
