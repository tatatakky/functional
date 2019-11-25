package monoids.monoidLaws

import org.scalatest.FunSpec

class MonoidLawsTest extends FunSpec {

  import monoids.Monoid.listMonoid

  describe("Monoid Laws (モノイド則)") {
    val list1: List[Int] = List(1, 2)
    val list2: List[Int] = List(3)
    val list3: List[Int] = List(4, 5, 6)

    it("Left identity (左単位元)") {
      assert( listMonoid.op(listMonoid.zero, list1) == list1)
    }

    it("Right identity (右単位元)") {
      assert( listMonoid.op(list1, listMonoid.zero) == list1)
    }

    it("Associativity (結合律)") {
      assert(
        listMonoid.op(listMonoid.op(list1, list2), list3)
          === listMonoid.op(list1, listMonoid.op(list2, list3))
      )
    }
  }
}