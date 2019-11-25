package parallel

import java.util.concurrent._
import java.util.concurrent.{
  Future => JFuture
} //JavaのFutureと区別するため、FutureをJFuture(Java Futureの略)として利用する。

object Par {

  def sum(ints: List[Int]): Int = ints.foldLeft(0)(_ + _)

  def sum2(ints: IndexedSeq[Int]): Int =
    if (ints.size <= 1)
      ints.headOption getOrElse 0
    else {
      val (l, r) = ints.splitAt(ints.length / 2)
      sum2(l) + sum2(r)
    }

  type Par[A] = ExecutorService => JFuture[A]

  def unit[A](a: A): Par[A] =
    es => UnitFuture(a)

  def lazyUnit[A](a: => A): Par[A] =
    fork(unit(a))

  def fork[A](a: => Par[A]): Par[A] =
    es => {
      es.submit(new Callable[A] {
        def call = {
//          println(Thread.currentThread())
          val x: A = a(es).get()
//          println(x)
          x
        }
      })
    }

  def sum_parInt(ints: IndexedSeq[Int]): Par[Int] =
    if (ints.size <= 1)
      unit(ints.headOption getOrElse 0)
    else {
      val (l, r) = ints.splitAt(ints.length / 2)
      map2(sum_parInt(l), sum_parInt(r))(_ + _)
    }

  //management of Task state, implement Future point abstract to concrete.
  case class UnitFuture[A](get: A) extends JFuture[A] {
    def cancel(mayInterruptIfRunning: Boolean): Boolean = false
    def get(timeout: Long, unit: TimeUnit): A = get
    def isCancelled: Boolean = false
    def isDone: Boolean = true
  }

  def map2[A, B, C](pa: Par[A], pb: Par[B])(f: (A, B) => C): Par[C] =
    es => {
      val fa: JFuture[A] = pa(es) //非同期実行
      val fb: JFuture[B] = pb(es) //非同期実行
      UnitFuture(f(fa.get, fb.get))
    }

  def run[A](s: ExecutorService)(a: Par[A]): JFuture[A] = a(s)

  def asyncF[A, B](f: A => B): A => Par[B] =
    a => lazyUnit(f(a))

  def map[A, B](pa: Par[A])(f: A => B): Par[B] =
    map2(pa, unit(()))((a, _) => f(a))

  def sortPar(parList: Par[List[Int]]): Par[List[Int]] =
    map(parList)(_.sorted)

  def sequence[A](ps: List[Par[A]]): Par[List[A]] =
    ps.foldRight(unit(List[A]()))((x, y) => map2(x, y)(_ :: _))

  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    val fbs: List[Par[B]] = ps.map(x => asyncF(f)(x))
    val seq: Par[List[B]] = sequence(fbs)
    seq
  }

  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
    val pars: List[Par[List[A]]] =
      as.map(asyncF((x: A) => if (f(x)) List(x) else List()))
    map(sequence(pars))(_.flatten)
  }

  def map3[A, B, C, D](pa: Par[A], pb: Par[B], pc: Par[C])(
      f: (A, B, C) => D): Par[D] =
    es => {
      val fa: JFuture[A] = pa(es)
      val fb: JFuture[B] = pb(es)
      val fc: JFuture[C] = pc(es)
      UnitFuture(f(fa.get, fb.get, fc.get))
    }

  def map4[A, B, C, D, E](pa: Par[A], pb: Par[B], pc: Par[C], pd: Par[D])(
      f: (A, B, C, D) => E): Par[E] =
    es => {
      val fa: JFuture[A] = pa(es)
      val fb: JFuture[B] = pb(es)
      val fc: JFuture[C] = pc(es)
      val fd: JFuture[D] = pd(es)
      UnitFuture(f(fa.get, fb.get, fc.get, fd.get))
    }

  def map5[A, B, C, D, E, F](pa: Par[A],
                             pb: Par[B],
                             pc: Par[C],
                             pd: Par[D],
                             pe: Par[E])(f: (A, B, C, D, E) => F): Par[F] =
    es => {
      val fa: JFuture[A] = pa(es)
      val fb: JFuture[B] = pb(es)
      val fc: JFuture[C] = pc(es)
      val fd: JFuture[D] = pd(es)
      val fe: JFuture[E] = pe(es)
      UnitFuture(f(fa.get, fb.get, fc.get, fd.get, fe.get))
    }

  def makeList(textFile: String): List[String] = {
    import scala.io.Source
    Source.fromFile(textFile)
      .getLines()
      .toList
  }

  def wordCount(textFile: String): Par[List[Int]] =
    parMap(makeList(textFile))(_.split(" ").length)

  def equal[A](e: ExecutorService)(p1: Par[A], p2: Par[A]): Boolean =
    p1(e).get == p2(e).get

  //恒等関数
  def id[A](a: A): A = a

  def lawOfId[A, B](x: A)(f: A => B): Boolean = {
    map(unit(x))(id) == unit(id(x))
  }

  def delay[A](fa: => Par[A]): Par[A] =
    es => fa(es)
}
