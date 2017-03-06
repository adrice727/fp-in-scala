
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

    def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
      a flatMap (aa => b map (bb => f(aa, bb)))
    }

    def sequence[A](a: List[Option[A]]): Option[List[A]] = {
      a.foldRight[Option[List[A]]](Some(Nil))((aa, acc) => map2(aa, acc)(_ :: _))
    }

    def sequence2[A](a: List[Option[A]]): Option[List[A]] = a match {
      case Nil => Some(Nil)
      case x :: xs => x flatMap (xx => sequence2(xs) map (xx :: _))
    }

    def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
      a.foldRight[Option[List[B]]](Some(Nil))((aa, acc) => map2(f(aa), acc)(_ :: _))
    }

    def sequenceViaTraverse[A](a: List[Option[A]]): Option[List[A]] = {
      traverse(a)(x => x)
    }
  }

  sealed trait Either[+E, +A] {
    def map[B](f: A => B): Either[E, B] = this match {
      case Right(a) => Right(f(a))
      case Left(e) => Left(e)
    }

    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
      case Left(e) => Left(e)
      case Right(a) => f(a)
    }

    def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
      case Left(_) => b
      case Right(a) => Right(a)
    }

    def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = {
      for {
        aa <- this
        bb <- b
      } yield f(aa, bb)
    }
  }

  case class Left[+E](value: E) extends Either[E, Nothing]
  case class Right[+A](value: A) extends Either[Nothing, A]

  object Either {
    def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = {
      es.foldRight[Either[E, List[A]]](Right(Nil))((ee, acc) => ee.map2(acc)(_ :: _))
    }

    def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = {
      as.foldRight[Either[E, List[B]]](Right(Nil))((a, acc) => f(a).map2(acc)(_ :: _))
    }
    
    def sequence2[E, A](es: List[Either[E, A]]): Either[E, List[A]] = traverse(es)(x => x)
  }
}




