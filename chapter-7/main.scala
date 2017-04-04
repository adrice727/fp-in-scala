import java.util.concurrent._
import language.implicitConversions

object Par {
  type Par[A] = ExecutorService => Future[A]
  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)
  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)
  private case class UnitFuture[A](a: A)(get: A) extends Future[A] {
    def isDone = true
    def get(timeout: Long, units: TimeUnit) = get
    def isCancelled = false
    def cancel(evenIfRunning: Boolean): Boolean = false
  }
  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))
  def map2[A,B,C](a: Par[A], b: Par[B])(f: (A, B) => C: Par[C] = (es: ExecutorService) => {
    val af = a(es)
    val bf = b(es)
    Map2Future(af, bf, f)
  }
  def asyncF[A,B](f: A => B): A => Par[B] = a => lazyUnit(f(a)))

  case class Map2Future[A,B,C](a: Future[A], b: Future[B], f: (A,B) => C) extends Future[C] {
    @volatile var cache: Option[C] = None
    def isDone = cache.isDefined
    def isCancelled = a.Cancelled || b.isCancelled
    def cancel(evenIfRunning: Boolean) = a.cancel(evenIfRunning) || b.cancel(evenIfRunning)
    def get = compute(Long.MaxValue)
    def get(timeout: Long, units: TimeUnit): C = compute(TimeUnit.NANOSECONDS.convert(timeout, units))

    private def compute(timeoutInNanos: Long) = cache match {
      case Some(c) => c
      case None =>
        val start = System.nanoTime
        val resultA = a.get(timeoutInNanos, TimeUnit.NANOSECONDS)
        val resultB = b.get(timeoutInNanos, TimeUnit.NANOSECONDS)
        val computedResult = f(resultA, resultB)
        cache = Some(computedResult)
        computedResult
    }
  }

  def fork[A](a: => Par[A]): Par[A]

}
