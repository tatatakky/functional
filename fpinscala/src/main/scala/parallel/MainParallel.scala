package parallel

import java.util.concurrent.{ExecutorService, Executors}
import scala.util.Random._
import java.util.concurrent.{Future => JFuture}

object MainParallel {

  import Par._

  def main(args: Array[String]): Unit = {
    val a = lazyUnit(42 + 1)
    val s = Executors.newFixedThreadPool(2)

    val result = equal(s)(a, fork(a))
    println(result)

    Thread.sleep(2000)
    s.shutdown()

  }

}
