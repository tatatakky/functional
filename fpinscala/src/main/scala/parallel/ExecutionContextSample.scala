package parallel

import scala.concurrent.ExecutionContext

object ExecutionContextSample {

  case class Add2Task(a: Int, b: Int) extends Runnable {

    def add(): Int = a + b

    def run(): Unit = println(s"[${Thread.currentThread()}] - $a + $b = ${add()}")

  }

  def main(args: Array[String]): Unit = {

    val ec: ExecutionContext = ExecutionContext.Implicits.global

    ec.execute(Add2Task(10, 20))
    ec.execute(Add2Task(40, 50))
    ec.execute(Add2Task(3, 27))
    ec.execute(Add2Task(5, 4))
    ec.execute(Add2Task(7, 28))
    ec.execute(Add2Task(12, 24))
    ec.execute(Add2Task(1000, 2000))
    ec.execute(Add2Task(1, 5))

    Thread.sleep(2000)

  }

}