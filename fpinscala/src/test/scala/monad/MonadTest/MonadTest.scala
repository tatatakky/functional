package monad

import org.scalatest.FunSpec

class MonadTest extends FunSpec {

  import monad.Id._
  import monad.State._
  import monad.Monad._

  describe("11.5 モナドとはいったい何か") {
    it("単位元モナド") {
      assert(
        Id("Hello, ").flatMap(a => Id("Monad!").flatMap(b => Id(a + b)))
          === Id("Hello, Monad!")
      )

      assert(
        (for{
          a <- Id("Hello, ")
          b <- Id("Monad!")
        } yield {
          a + b
        }) === Id("Hello, Monad!")
      )
    }

    it("State モナドと部分的な型の適用") {
      val s = State((x: Int) => (x.toString, x*3))

      val res_map = s.map(str => Id(str)).run(1)
      assert( res_map === (Id("1"), 3) )

      val res_flatMap = s.flatMap(str => State((x: Int) => (Id(str), x*2))).run(1)
      assert( res_flatMap === (Id("1"), 6) )

      val state1 = State((x: Int) => (x.toString, x * 2))
      val state2 = State((y: Int) => (y.toString, y * 7))

      val res_state_monad_replicateM = stateMonad.replicateM(3, s).run(3)
      assert(res_state_monad_replicateM === (List("3", "9", "27"), 81) )

      val state_monad_map2 = stateMonad.map2(state1, state2)(_ + _).run(3)
      assert( state_monad_map2 === ("36", 42) )

      val state_monad_seq = stateMonad.sequence(List(state1, state2)).run(3)
      assert( state_monad_seq === (List("3", "6"), 42))

      val state_monad_zipWithIndex = zipWithIndex(List("a", "b", "c"))
      assert( state_monad_zipWithIndex === List((0, "a"), (1, "b"), (2, "c")))

    }
  }
}
