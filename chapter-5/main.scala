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
    case Cons(h, t) if n > 0 => cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case _ => empty
  }

  @tailrec
  final def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _ => empty
  }

  def takeWhile(f: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if f(h()) => cons(h(), t().takeWhile(f))
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

  def apply[A](as: A*): Stream[A] = {
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
  }
}