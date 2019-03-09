package fpinscala.datastructures



sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  def size[A](t : Tree[A]): Int = {
    t match {
      case Leaf(_) => 1
      case Branch(l, r) => 1 + size(l) + size(r)
    }
  }

  def maximum(t : Tree[Int]): Int = {
    t match {
      case Leaf(v) => v
      case Branch(l, r) => Math.max(maximum(l), maximum(r))
    }
  }

  def depth[A](t : Tree[A]) : Int = {
    t match {
      case Leaf(_) => 0
      case Branch(l, r) => 1 + Math.max(depth(l), depth(r))
    }
  }

  def map[A, B](t : Tree[A])(f : A => B) : Tree[B] = {
    t match {
      case Leaf(v) => Leaf(f(v))
      case Branch(l, r) => Branch(map(l)(f), map(r)(f))
    }
  }

  def fold[A, B](t : Tree[A])(g : A => B)(f : (B, B) => B) : B = {
    t match {
      case Leaf(a) => g(a)
      case Branch(l, r) => f(fold(l)(g)(f), fold(r)(g)(f))
    }
  }
  def sizeWithFold[A](t : Tree[A]): Int = {
    fold(t)(_ => 1)((s1, s2) => 1 + s1 + s2)
  }

  def maximumWithFold(t : Tree[Int]): Int = {
    fold(t)(identity)((s1, s2) => Math.max(s1, s2))
  }

  def depthWithFold[A](t : Tree[A]) : Int = {
    fold(t)(_  => 0)((m1, m2) => 1 + Math.max(m1, m2))
  }

  def mapWithFold[A, B](t : Tree[A])(f : A => B) : Tree[B] = {
    fold(t)(x => Leaf(f(x)) : Tree[B])((b1, b2) => Branch(b1, b2))
  }

}

object TreeTests extends App {
  //import Tree._
  import Tree._
  val a = Branch(
    Branch(Leaf(1), Leaf(10)),
    Leaf(-3)
  )
  //val res = depth(a)
  val f = (x : Int) => (x * x).toString + (x * x).toString
  val res = map(a)(f)
  println(res)

  println(sizeWithFold(a))
  println(maximumWithFold(a))
  println(depthWithFold(a))
  println(mapWithFold(a)(f))

}