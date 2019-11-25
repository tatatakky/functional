package monad.monadLaws

import org.scalatest.FunSpec

class MonadLawsTest extends FunSpec {

  import monad.Monad.{listMonad, optionMonad}
  import monad.Functor.listFunctor

  describe("Functor Laws (ファンクタ則)") {
    val fa = listFunctor
    val list: List[Int] = List(1, 2, 3)
//    def identity: Int => Int = x => x
    def identity[A](a: A): A = a
    it("identity (単位元)") {
      assert( fa.map(list)(identity) === list)
    }
  }

  describe("Monad Laws (モナド則)") {

    case class Number(a: Int)
    case class DoubledNumber(b: Int)
    val lv: List[Int] = List(2)
    val v: Int = 2
    val listF: Int => List[Number] = (value: Int) => List(Number(value))
    val listG: Number => List[DoubledNumber] = (value: Number) => List(DoubledNumber(value.a * 2))

    it("Left identity (同一律: 左単位元)") {
      assert( listMonad.flatMap(listMonad.unit(v))(listF) === listF(v) )
    }

    it("Right identity (同一律: 右単位元)") {
      assert( listMonad.flatMap(lv)(listMonad.unit(_)) === lv)
    }

    it("Associativity (結合律)") {
      assert(
        listMonad.flatMap(listMonad.flatMap(lv)(listF))(listG)
          === listMonad.flatMap(lv)(t => listMonad.flatMap(listF(t))(listG))
      )
    }
  }

}