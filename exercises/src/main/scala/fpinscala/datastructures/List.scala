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


  def fill[A](n : Int)(a : => A) : List[A] = {
    @tailrec
    def loop(n: Int, acc: List[A]): List[A] = {
      if (n == 0) acc
      else loop(n - 1, Cons(a, acc))
    }
    loop(n, Nil)
  }



  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

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
    else drop(tail(l), n-1)

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
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B =
    l match {
      case Nil => z
      case Cons(head, xs) => foldLeft(xs, f(z, head))(f)
    }

  def reverse[A](l : List[A]) : List[A] = foldLeft[A, List[A]](l, Nil)((acc, h) => Cons(h, acc))

  def foldLeft2[A,B](l: List[A], z: B)(f: (B, A) => B): B =  foldRight(reverse(l), z)((a,b) => f(b,a))
  def foldRight2[A,B](l: List[A], z: B)(f: (A, B) => B): B =  foldLeft(reverse(l), z)((a,b) => f(b,a))

  def length2[A](l: List[A]): Int = foldRight2(l, 0)((_, s) => 1 + s)


  def map[A,B](l: List[A])(f: A => B): List[B] = ???
}

object ListMain extends App {
  val l1 = List(1, 2, 3, 4)
  val l2 = Nil
  val l3 = List(5, 6, 7, 8)
  val l4 = List(1)

  import List._

  val l5 = fill(10000000)(3)


  //
//  println(tail(l1))
//  println(setHead(l1, 42))
//  println(drop(l1, 2))
//  println(dropWhile(l1, (x : Int)  => x <= 3))
//  println(init(l1))
//
//
//  println(tail(l2))
//  println(setHead(l2, 42))
//  println(drop(l2, 2))
//  println(dropWhile(l2, (x : Int)  => x <= 2))
//  println(init(l2))
//
//  println(init(l4))
//
//  println(append(l1, l3))


  print(length2(l5))

}
