package state

import org.scalatest.FunSpec

class StateSpec extends FunSpec{

  import state.RNG._

  describe("State"){

    it("random"){
      val rng: SimpleRNG = SimpleRNG(42)
      val (n1, rng2) = rng.nextInt
      val (n2, rng3) = rng2.nextInt
      assert(n2 === rng.nextInt._2.nextInt._1)
    }
  }
}
