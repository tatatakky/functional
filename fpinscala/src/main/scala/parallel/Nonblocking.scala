package parallel

import java.util.concurrent.{CountDownLatch, ExecutorService, Future => JFuture}
import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.Callable

object Nonblocking {

  trait JFuture[A]{
    private[parallel] def apply(k: A => Unit): Unit
  }

  type Par[A] = ExecutorService => JFuture[A]

  def run[A](es: ExecutorService)(p: Par[A]): A = {

    val ref   = new AtomicReference[A]
    val latch = new CountDownLatch(1)
    p(es){
      a => ref.set(a); latch.countDown()
    }
    latch.wait
    ref.get
  }

  def unit[A](a: A): Par[A] =
    es => new JFuture[A] {
      def apply(callback: A => Unit): Unit =
        callback(a)
    }

  def fork[A](a: => Par[A]): Par[A] =
    es => new JFuture[A]{
      def apply(callback: A => Unit): Unit =
        eval(es)(a(es)(callback))
    }

  def eval(es: ExecutorService)(r: => Unit): Unit =
    es.submit(new Callable[Unit]{
      def call = r
    })
}