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

  def mapWithUnfold[B](f: A => B): Stream[B] =
    unfold(this) {
      case Cons(h, t) => Some(f(h()), t())
      case _ => None
    }

  def takeWithUnfold(n: Int): Stream[A] =
    unfold((this, n)) {
      case (Cons(h, t), 1) => Some(h(), (empty, 0))
      case (Cons(h, t), n) if n > 1 => Some(h(), (t(), n - 1))
      case _ => None
    }

  def takeWhileWithUnfold(f: A => Boolean): Stream[A] =
    unfold(this) {
      case Cons(h, t) if f(h()) => Some((h(), t()))
      case _ => None
    }

  def zipWith[B, C](s: Stream[B])(f: (A, B) => C): Stream[C] =
    unfold((this, s)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some(f(h1(), h2()), (t1(), t2()))
      case _ => None
    }

  def zipAll[B](s: Stream[B]): Stream[(Option[A], Option[B])] = {
    unfold((this, s)) {
      case (Empty, Empty) => None
      case (Cons(h1, t1), Empty) => Some((Some(h1()), None), (t1(), empty))
      case (Empty, Cons(h2, t2)) => Some((None, Some(h2())), (empty, t2()))
      case (Cons(h1, t1), Cons(h2, t2)) => Some((Some(h1()), Some(h2())), (t1(), t2()))
    }
  }

  /**
    * 1. Zip both streams together into a stream of tuples => [(Option[A], Option[B])
    * 2. Create a stream of the first n tuples where `this` has values remaining
    * 3. For the remaining tuples, see if the heads match using forAll
    */
  def startsWith[A](s: Stream[A]): Boolean = {
    zipAll(s).takeWhile(!_._2.isEmpty) forAll {
      case (h1, h2) => h1 == h2
    }
  }

  //  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
  //    case Some((a, s)) => cons(a, unfold(s)(f))
  //    case None => empty
  //  }

  def tails: Stream[Stream[A]] = {
    unfold(this) {
      case Empty => None
      case s => Some(s, s drop 1)
    }
  }

  def hasSubsequence[A](s: Stream[A]): Boolean = {
    tails.exists(_ startsWith s)
  }

  def mapTails[B](f: A => B): Stream[Stream[B]] = {
    unfold(this) {
      case Empty => None
      case s => Some(s map f, s drop 1)
    }
  }

  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =
    foldRight((z, Stream(z)))((a, acc) => {
      lazy val acc1 = acc
      val a2 = f(a, acc1._1)
      (a2, cons(a2, acc1._2))
    })._2
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
