package monoid

import org.scalatest.FunSpec

class MonoidSpec extends FunSpec {
  describe("Monoid") {
    val x: IndexedSeq[Int] = IndexedSeq(1, 2, 3, 4)
    val l: List[Int] = List(1, 2, 3, 4)
    it("foldMapV function works as well") {
      assert(Monoid.foldMapV(x, Monoid.intAddition)(_ * 2) === 20)
    }

    it("composability of Monoid") {
      import Monoid._
      assert( FoldableList.foldMap(l)(x => (1, x))(productMonoid(intAddition, intAddition)) === (4, 10) )
    }
  }
}