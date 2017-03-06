
import scala.{Option => _, Some => _, Either => _, _}

object Main {

  sealed trait Option[+A] {
    def map[B](f: A => B): Option[B] = this match {
      case None => None
      case Some(a) => Some(f(a))
    }

    def getOrElse[B >: A](default: => B): B = this match {
      case None => default
      case Some(a) => a
    }

    def flatMap[B](f: A => Option[B]): Option[B] = map(f) getOrElse None

    def orElse[B >: A](ob: => Option[B]): Option[B] = map(Some(_)) getOrElse ob

    def filter(f: A => Boolean): Option[A] = flatMap(a => if (f(a)) Some(a) else None)

  }

  case class Some[+A](get: A) extends Option[A]
  case object None extends Option[Nothing]

  object Option {
    def mean(xs: Seq[Double]): Option[Double] = if (xs.isEmpty) None else Some(xs.sum / xs.length)

    def variance(xs: Seq[Double]): Option[Double] = {
      mean(xs) flatMap (m => mean(xs.map(x => math.pow(x - m, 2))))
    }

    def map2[A,B,C](a: Option[A], b: Option[B])(f: (A,B) => C): Option[C] =  {
      a flatMap(aa => b map (bb => f(aa,bb)))
    }

    def sequence[A](a: List[Option[A]]): Option[List[A]] = {
      a.foldRight[Option[List[A]]](Some(Nil))((aa, acc) => map2(aa,acc)(_ :: _))
    }

    def sequence2[A](a: List[Option[A]]): Option[List[A]] = a match  {
      case Nil => Some(Nil)
      case x :: xs => x flatMap (xx => sequence2(xs) map (xx :: _))
    }

    def traverse[A,B](a: List[A])(f: A => Option[B]): Option[List[B]] =  {
      a.foldRight[Option[List[B]]](Some(Nil))((aa, acc) => map2(f(aa), acc)(_ :: _))
    }

    def sequenceViaTraverse[A](a: List[Option[A]]): Option[List[A]] = {
      traverse(a)(x => x)
    }
  }

}

