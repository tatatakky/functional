package monad

import org.scalatest.FunSpec

class MonadOptionTest extends FunSpec {

  import monad.Monad.optionMonad._

  val a = 1
  val f = (a: Int) => Some(a)
  val g = (b: Int) => Some(b.toDouble)
  val h = (c: Double) => Some(c.toString)

  val x: Option[Int] = Some(1)
  val y = 2

  val func = (v: Option[Int]) => Option(v)

  describe("fp in Scala - Monad") {

    it("Ex11.9 flatMapの観点からの結合律とcomposeの観点からの結合律の式が等価であることを証明せよ。") {
      assert( compose(compose(f, g), h)(a) == compose(f, compose(g, h))(a) )
    }

//    it("Ex11.10 同一律のcompose, flatMap, 2つのステートメントが同等であることを証明せよ。") {
//      assert( compose(f, unit)(a) == f(a) )
//      assert( compose(unit, f) == f )
//      assert( flatMap(x)(unit) == x )
//      assert( flatMap(unit(y))(f) == f(y) )
//    }

    it("Ex11.11 あなたが選んだモナドで同一律が成り立つことを証明せよ。") {
      assert( flatMap(None)(Some(_)) === None )
      assert( flatMap(Some(1))(Some(_)) === Some(1) )
      assert( flatMap(Some(1))(f) === f(1))
      assert( flatMap(Some(None))(func) === func(None) )
    }
  }

}