import scala.annotation.tailrec

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def setHead[A](l: List[A], a: A): List[A] = l match {
    case Nil => List(a)
    case Cons(_, xs) => Cons(a, xs)
  }

  def tail[A](l: List[A]): List[A] = drop(l, 1)

  @tailrec
  def drop[A](l: List[A], n: Int): List[A] = l match {
    case Nil => Nil
    case Cons(_, xs) => if (n == 0) l else drop(xs, n - 1)
  }

  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Cons(x, xs) if f(x) => dropWhile(xs)(f)
    case _ => l
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(x, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  def sum2(ns: List[Int]) = foldRight(ns, 0)(_ + _)

  def product2(ns: List[Double]) = foldRight(ns, 1.0)(_ * _)

  def length[A](l: List[A]): Int = foldRight(l, 0)((_, acc) => acc + 1)

  @tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  def sum3(ns: List[Int]) = foldLeft(ns, 0)(_ + _)

  def product3(ns: List[Double]) = foldLeft(ns, 1.0)(_ * _)

  def length2[A](l: List[A]): Int = foldLeft(l, 0)((acc, _) => acc + 1)

  def reverse[A](l: List[A]): List[A] = foldLeft(l, List[A]())((acc, a) => Cons(a, acc))

  // Using foldLeft
  def foldRight2[A, B](l: List[A], z: B)(f: (A, B) => B): B = {
    foldLeft(l, (b: B) => b)((acc, a) => b => acc(f(a, b)))(z)
  }

  // Using foldRight
  def foldLeft2[A, B](l: List[A], z: B)(f: (B, A) => B): B = {
    foldRight(l, (b: B) => b)((a, acc) => b => acc(f(b, a)))(z)
  }

  def append[A](l1: List[A], l2: List[A]): List[A] = foldRight(l1, l2)(Cons(_, _))

  // Take a list of lists
  def concat[A](l: List[List[A]]): List[A] = foldRight(l, List[A]())(append)

  // Variadic version
  def concat2[A](l: List[A]*): List[A] = {
    if (l.isEmpty) Nil
    else append(l.head, concat2(l.tail: _*))
  }

  // Add one to each value in the list
  def addOne(ints: List[Int]): List[Int] = ints match {
    case Nil => Nil
    case Cons(x, xs) => Cons(x + 1, addOne(xs))
  }

  // Using fold
  def addOne2(ints: List[Int]): List[Int] = {
    foldRight(ints, List[Int]())((a, acc) => Cons(a + 1, acc))
  }

  // Convert a list of doubles to a list of strings
  def doublesToStrings(dbs: List[Double]): List[String] = dbs match {
    case Nil => Nil
    case Cons(x, xs) => Cons(x.toString, doublesToStrings(xs))
  }

  // Using fold
  def doublesToStrings2(dbs: List[Double]): List[String] = {
    foldRight(dbs, List[String]())((a, acc) => Cons(a.toString, acc))
  }

  // Using pattern matching
  def map[A, B](l: List[A])(f: A => B): List[B] = l match {
    case Nil => Nil
    case Cons(x, xs) => Cons(f(x), map(xs)(f))
  }

  // Using stack-safe foldRight
  def map2[A, B](l: List[A])(f: A => B): List[B] = {
    foldRight2(l, List[B]())((a, acc) => Cons(f(a), acc))
  }

  // Using a List Buffer
  def map3[A, B](l: List[A])(f: A => B): List[B] = {
    val buf = new collection.mutable.ListBuffer[B]
    def buildBuffer(l: List[A]): Unit = l match {
      case Nil => ()
      case Cons(x, xs) => buf += f(x); buildBuffer(xs)
    }
    buildBuffer(l)
    List(buf.toList: _*) // converting from the standard Scala list to the list we've defined here
  }

  // Using stack-safe foldRight
  def filter[A](l: List[A])(f: A => Boolean): List[A] = {
    foldRight2(l, List[A]())((a, acc) => if (f(a)) Cons(a, acc) else acc)
  }

  def removeOdds(ints: List[Int]): List[Int] = filter(ints)(_ % 2 == 0)

  def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] = {
    concat(map(l)(f))
  }

  // Using flatMap
  def filter2[A](l: List[A])(f: A => Boolean): List[A] = {
    flatMap(l)(i => if (f(i)) Cons(i, Nil) else Nil)
  }

  def sumPairs(ints1: List[Int], ints2: List[Int]): List[Int] = (ints1, ints2) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(x1, xs1), Cons(x2, xs2)) => Cons(x1 + x2, sumPairs(xs1, xs2))
  }

  def zipWith[A, B, C](l1: List[A], l2: List[B])(f: (A, B) => C): List[C] = (l1, l2) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(x1, xs1), Cons(x2, xs2)) => Cons(f(x1, x2), zipWith(xs1, xs2)(f))
  }

  @tailrec
  def startsWith[A](l: List[A], prefix: List[A]): Boolean = (l, prefix) match {
    case (_, Nil) => true
    case (Cons(lx, lxs), Cons(px, pxs)) if lx == px => startsWith(lxs, pxs)
    case _ => false
  }

  @tailrec
  def hasSubsequence[A](l: List[A], sub: List[A]): Boolean = l match {
    case Nil => sub == Nil
    case _ if startsWith(l, sub) => true
    case Cons(_, xs) => hasSubsequence(xs, sub)
  }

  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }
}

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + size(l) + size(r)
  }

  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(x) => x
    case Branch(l, r) => maximum(l) max maximum(r)
  }

  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 0
    case Branch(l, r) => 1 + (depth(l) max depth(r))
  }

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(x) => Leaf(f(x))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = t match {
    case Leaf(x) => f(x)
    case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
  }

  def size2[A](t: Tree[A]): Int = fold(t)(_ => 1)(1 + _ + _)

  def maximum2(t: Tree[Int]): Int = fold(t)(a => a)(_ max _)

  def depth2[A](t: Tree[A]): Int = fold(t)(_ => 0)((d1, d2) => 1 + (d1 max d2))

  def map2[A, B](t: Tree[A])(f: A => B): Tree[B] = fold(t)(a => Leaf(f(a)): Tree[B])(Branch(_, _))
}

