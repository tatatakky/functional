package parallel

import parallel.Par._
import org.scalatest.FunSpec
import java.util.concurrent.Executors

class ParMapSpec extends FunSpec {

  describe("Par implementation") {
    it("parMap") {
      val list: List[Int] = List(1,2,3,4)
      val ec = Executors.newFixedThreadPool(2)
      val res = Par.run(ec)(parMap(list)(_ * 2)).get
      ec.shutdown()
      assert(res == List(2, 4, 6, 8))
    }
  }
}