package parallel

import java.util.concurrent.{Executors, ExecutorService, Callable}
import java.util.concurrent.{Future => JFuture}

case class MultiTask(a: Int, b: Int) extends Callable[Int]{
  def call(): Int = {
    println(s"${Thread.currentThread()}")
    a * b
  }
}

object FutureCallableSample {

  def main(args: Array[String]): Unit = {
    val exes: ExecutorService = Executors.newFixedThreadPool(3)

    val f1: JFuture[Int] = exes.submit(MultiTask(2, 3))
    val f2: JFuture[Int] = exes.submit(MultiTask(3, 4))
    val f3: JFuture[Int] = exes.submit(MultiTask(4, 5))
    val f4: JFuture[Int] = exes.submit(MultiTask(5, 6))
    val f5: JFuture[Int] = exes.submit(MultiTask(6, 7))
    val f6: JFuture[Int] = exes.submit(MultiTask(7, 8))

    println(f1.get())
    println(f2.get())
    println(f3.get())
    println(f4.get())
    println(f5.get())
    println(f6.get())


//    println(exes.submit(MultiTask(2, 3)).get())
//    println(exes.submit(MultiTask(3, 4)).get())
//    println(exes.submit(MultiTask(4, 5)).get())
//    println(exes.submit(MultiTask(5, 6)).get())
//    println(exes.submit(MultiTask(6, 7)).get())
//    println(exes.submit(MultiTask(7, 8)).get())

    Thread.sleep(2000)
    exes.shutdown()

  }

}