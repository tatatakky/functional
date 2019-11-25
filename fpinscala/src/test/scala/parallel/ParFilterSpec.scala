package parallel

import org.scalatest.FunSpec
import java.util.concurrent.Executors
import parallel.Par.parFilter

class ParFilterSpec extends FunSpec {

  describe("Par Implementation 2"){

    it("parFilter"){
      val list: List[Int] = List(1,2,3,4,5,6)
      val ec = Executors.newFixedThreadPool(3)
      val res = Par.run(ec)(parFilter(list)(_ % 2 == 0)).get
      ec.shutdown()
      assert(res == List(2, 4, 6))
    }
  }
}