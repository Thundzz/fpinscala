package fpinscala.errorhandling


import scala.{Either => _, Left => _, Option => _, Right => _} // hide std library `Option` and `Either`, since we are writing our own in this chapter

sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B] = this match {
    case Left(e) => Left(e)
    case Right(a) => Right(f(a))
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Left(e) => Left(e)
    case Right(a) => f(a)
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Left(_) => b
    case Right(a) => Right(a)
  }


  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
  //for { a <- this; b1 <- b } yield f(a,b1)
    this.flatMap[EE, C](a => b.map(bb => f(a, bb)))
}

case class Left[+E](get: E) extends Either[E, Nothing]

case class Right[+A](get: A) extends Either[Nothing, A]

object Either {
  def traverse[E, A, B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] = es match {
    case Nil => Right(Nil)
    case a :: as => for {
      b <- f(a)
      bs <- traverse(as)(f)
    } yield b :: bs
  }

  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
    traverse(es)(identity)

  def mean(xs: IndexedSeq[Double]): Either[String, Double] =
    if (xs.isEmpty)
      Left("mean of empty list!")
    else
      Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Either[Exception, Int] =
    try Right(x / y)
    catch {
      case e: Exception => Left(e)
    }

  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch {
      case e: Exception => Left(e)
    }

}

object EitherMain extends App {


  def sum(x: Int, y: Int): Int = {
    x + y
  }


  val as = List("1", "2", "3", "banane", "5")
  val bs = List("1", "2", "3", "7", "5")

  val a = Right(1)
  val b = Left(2)



  println(a.map2(b)(sum))
  println(Either.traverse(as)((x : String) =>  Either.Try(x.toInt)))
  println(Either.sequence(as.map(x=> Either.Try(x.toInt))))

  println(Either.traverse(bs)((x : String) =>  Either.Try(x.toInt)))
  println(Either.sequence(bs.map(x=> Either.Try(x.toInt))))
}