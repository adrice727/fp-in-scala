import scala.annotation.tailrec

object Main {
  def fib(n: Int): Int = {
    @tailrec
    def next(n: Int, a: Int = 0, b: Int = 1): Int = {
      if (n == 0) a
      else next(n - 1, b, a + b)
    }
    next(n - 1)
  }

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @tailrec
    def next(x: A, xs: Array[A]): Boolean = {
      if (xs.length == 0) true
      else if (!ordered(x, xs.head)) false
      else next(xs.head, xs.tail)
    }
    if (as.length == 0) true else next(as.head, as.tail)
  }

  def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
    (a: A) => (b: B) => f(a, b)
  }

  def uncurry[A,B,C](f: A => B => C): (A,B) => C  = {
    (a: A, b: B) => f(a)(b)
  }

  def compose[A,B,C](f: B => C, g: A => B): A => C = {
    (a: A) => f(g(a))
  }
}