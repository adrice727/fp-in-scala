import scala.annotation.tailrec

import Stream._

sealed trait Stream[+A] {
  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }

  def toList: List[A] = this match {
    case Cons(h, t) => h() :: t().toList
    case _ => List()
  }

  def toList2: List[A] = {
    @tailrec
    def buildList(s: Stream[A], acc: List[A]): List[A] = s match {
      case Empty => acc
      case Cons(h, t) => buildList(t(), h() :: acc)
    }
    buildList(this, List()).reverse
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case _ => empty
  }

  @tailrec
  final def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _ => this
  }

  def takeWhile(f: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if f(h()) => cons(h(), t().takeWhile(f))
    case _ => empty
  }

  def exists(f: A => Boolean): Boolean = this match {
    case Cons(h, t) => f(h()) || t().exists(f)
    case _ => false
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def exists2(f: A => Boolean): Boolean = this.foldRight(false)((a, acc) => f(a) || acc)

  def forAll(f: A => Boolean): Boolean = this.foldRight(true)((a, acc) => f(a) && acc)

  def takeWhile2(f: A => Boolean): Stream[A] = {
    this.foldRight(empty[A])((a, acc) => if (f(a)) cons(a, acc) else acc)
  }

  def headOption2: Option[A] = foldRight(None: Option[A])((a, acc) => Some(a))

  def map[B](f: A => B): Stream[B] = foldRight(empty[B])((a, acc) => cons(f(a), acc))

  def filter(f: A => Boolean): Stream[A] = foldRight(empty[A])((a, acc) => if (f(a)) cons(a, acc) else acc)

  def append[B >: A](s: => Stream[B]): Stream[B] = foldRight(s)(cons(_, _))

  def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight(empty[B])((a, acc) => f(a) append acc)
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

  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  def fibs(a: Int = 0, b: Int = 1): Stream[Int] = cons(a, fibs(b, a + b))

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((a, s)) => cons(a, unfold(s)(f))
    case None => empty
  }

  def onesWithUnfold: Stream[Int] = unfold(1)(_ => Some(1, 1))

  def constantWithUnfold[A](a: A): Stream[A] = unfold(a)(_ => Some(a, a))

  def fromWithUnfold(n: Int): Stream[Int] = unfold(n)(_ => Some(n, n + 1))

  def fibsWithUnfold: Stream[Int] = unfold((0, 1)) { case (a, b) => Some((a, (b, a + b))) }

  def apply[A](as: A*): Stream[A] = {
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
  }
}
