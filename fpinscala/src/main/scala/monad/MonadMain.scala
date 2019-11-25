package monad

object MonadMain {

  import monad.State._

  def main(args: Array[String]): Unit = {
    val s = State((x: Int) => (x.toString, x*3))

    val res = s.map(_ + " is String").run(1)
    println(res)

    val res_flatMap = s.flatMap(str => State((x: Int) => (Id(str), x*2))).run(1)
    println(res_flatMap)


  }

}