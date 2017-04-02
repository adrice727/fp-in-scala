import java.util.concurrent.ExecutorService

object Par {
  type Par[A] = ExecutorService => Future[A]
  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)
  def unit[A](a: A): Par[A]
  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))
  def map2[A,B,C](a: Par[A], b: Par[B])(f: (A, B) => C: Par[C]
  def fork[A](a: => Par[A]): Par[A]

}
