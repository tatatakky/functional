package parallel

import java.util.concurrent._

case class AddTask(first: Int, second: Int) extends Runnable {
  def add(): Int = first + second
  def run(): Unit = {
    println(s"[${Thread.currentThread()}] - $first + $second = ${add()}")
  }
}

object ExecutorServiceSample {

  def main(args: Array[String]): Unit = {

    val executor: ExecutorService = Executors.newFixedThreadPool(6)

    executor.submit(AddTask(1, 2))
    executor.submit(AddTask(3, 4))
    executor.submit(AddTask(5, 6))
    executor.submit(AddTask(7, 8))
    executor.submit(AddTask(9, 10))
    executor.submit(AddTask(100, 200))
    executor.submit(AddTask(300, 400))
    executor.submit(AddTask(500, 600))
    executor.submit(AddTask(700, 800))
    executor.submit(AddTask(800, 900))

    executor.submit(new Runnable {
      def run(): Unit = println(s"[${Thread.currentThread()}]")
    })

    Thread.sleep(2000)
    executor.shutdown()
  }
}


//The Output is like following.

//[Thread[pool-10-thread-4,5,run-main-group-0]] - 7 + 8 = 15
//[Thread[pool-10-thread-2,5,run-main-group-0]] - 3 + 4 = 7
//[Thread[pool-10-thread-4,5,run-main-group-0]] - 100 + 200 = 300
//[Thread[pool-10-thread-1,5,run-main-group-0]] - 1 + 2 = 3
//[Thread[pool-10-thread-4,5,run-main-group-0]] - 500 + 600 = 1100
//[Thread[pool-10-thread-3,5,run-main-group-0]] - 5 + 6 = 11
//[Thread[pool-10-thread-5,5,run-main-group-0]] - 9 + 10 = 19
//[Thread[pool-10-thread-4,5,run-main-group-0]] - 800 + 900 = 1700
//[Thread[pool-10-thread-3,5,run-main-group-0]]
//[Thread[pool-10-thread-2,5,run-main-group-0]] - 300 + 400 = 700
//[Thread[pool-10-thread-1,5,run-main-group-0]] - 700 + 800 = 1500


