
trait RNG {
  def nextInt: (Int, RNG)
}

object RNG {
  case class SimpleRNG(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = SimpleRNG(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }
  }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, r) = rng.nextInt
    (if (i < 0) -(i + 1) else i, r)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (i, r) = nonNegativeInt(rng)
    (i / (Int.MaxValue.toDouble + 1), r)
  }
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, rng2) = rng.nextInt
    val (d, rng3) = double(rng2)
    ((i, d), rng3)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((i, d), rng2) = intDouble(rng)
    ((d, i), rng2)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d, rng2) = double(rng)
    val (d2, rng3) = double(rng2)
    val (d3, rng4) = double(rng3)
    ((d, d2, d3), rng4)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    if (count == 0) {
      (List(), rng)
    } else {
      val (x, rng2) = rng.nextInt
      val (xs, rng3) = ints(count - 1)(rng2)
      (x :: xs, rng3)
    }
  }
  def ints2(count: Int)(rng: RNG): (List[Int], RNG) = {
    def go(count: Int, r: RNG, xs: List[Int]): (List[Int], RNG) = {
      if (count == 0) {
        (xs, r)
      } else {
        val (x, r2) = r.nextInt
        go(count - 1, r2, x :: xs)
      }
    }
    go(count, rng, List())
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] = rng => {
    val (a, rng2) = s(rng)
    (f(a), rng2)
  }

  def nonNegativeEven: Rand[Int] = map(nonNegativeInt)(i => i - i % 2)

  val doubleViaMap: Rand[Double] = map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A,B) => C): Rand[C] = rng => {
    val (a1, r1) = ra(rng)
    val (b1, r2) = rb(r1)
    (f(a1, b1), r2)
  }

  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = map2(ra, rb)((_, _))

  val randIntDouble: Rand[(Int, Double)] = both(int, double)

  val randDoubleInt: Rand[(Double, Int)] = both(double, int)

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = rng => {
    fs.foldRight(unit(List[A]()))((f, acc) => map2(f,acc)(_ :: _))
  }

  def intsViaSequence(count: Int): Rand[List[Int]] = {
    sequnce(List.fill(count)(int))
  }
}