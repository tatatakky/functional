package applicative.applicativeLaws

import org.scalatest.FunSpec

class ApplicativeLawsTest extends FunSpec {

  import applicative.Applicative

  describe("Applicative Laws (アプリカティブ則)") {

    val optionApplicative = new Applicative[Option] {
      def unit[A](a: => A): Option[A] = Some(a)
      override def map2[A, B, C](fa: Option[A], fb: Option[B])(f: (A, B) => C): Option[C] =
        (fa, fb) match {
          case (Some(a), Some(b)) => Some(f(a, b))
          case _ => None
        }
    }

    val fa: Option[Int] = Some(1)
    val fb: Option[Int] = Some(2)
    val fc: Option[Int] = Some(3)

    it("Left identity (左単位元)") {
      assert(
        optionApplicative.map2(optionApplicative.unit(()), fa)((_, a) => a)
          === fa
      )
    }

    it("Right identity (右単位元)") {
      assert(
        optionApplicative.map2(fa, optionApplicative.unit(()))((a, _) => a)
          === fa
      )
    }

    it("Associativity (結合律)") {
      assert(
        optionApplicative.product(optionApplicative.product(fa, fb), fc)
          === optionApplicative.map(optionApplicative.product(fa, optionApplicative.product(fb, fc)))(optionApplicative.assoc)
      )
    }

    it("Naturality (自然性)") {
      val F: Applicative[Option] = new Applicative[Option] {
        def unit[A](a: => A): Option[A] = Some(a)
        override def map2[A, B, C](fa: Option[A], fb: Option[B])(f: (A, B) => C): Option[C] =
          (fa, fb) match {
            case (Some(a), Some(b)) => Some(f(a, b))
            case _ => None
          }
      }

      case class Employee(name: String, id: Int)
      case class Pay(rate: Double, hoursPerYear: Double)

      def format(e: Option[Employee], pay: Option[Pay]): Option[String] =
        F.map2(e, pay){
          (e, pay) =>
            s"${e.name} makes ${pay.rate * pay.hoursPerYear}"
        }

      val e: Option[Employee] = Some(Employee("foo", 12345))
      val pay: Option[Pay] = Some(Pay(0.8, 50.0))

      assert( format(e, pay) === Some("foo makes 40.0") )

      def _format(name: Option[String], pay: Option[Double]): Option[String] =
        F.map2(name, pay)((n, p) => s"$n makes $p")

      val name: Option[String] = F.map(e)(_.name)
      val ppay: Option[Double] = F.map(pay)(p => p.rate * p.hoursPerYear)

      assert( _format(name, ppay) === Some("foo makes 40.0") )

      val a: Option[Int] = Some(4)
      val b: Option[Int] = Some(5)

      val f: Int => Option[String] = x => F.map(F.unit(x))(_.toString)
      val g: Int => Option[Double] = x => F.map(F.unit(x))(_.toDouble)

      assert(
        F.map2(a, b)(F.productF(f, g))
          === F.product(F.map(a)(f), F.map(b)(g))
      )
    }
  }

  describe("Ex12.7 モナド則が適用される場合、map2とmapのMonad実装がアプリカティブの法則を満たすことを明らかにするという方法で、全てのモナドがアプリカティブファンクタであることを証明せよ。") {

    import applicative.Monad

    val M: Monad[Option] = new Monad[Option] {
      def unit[A](a: => A): Option[A] = Some(a)
      def flatMap[A, B](ma: Option[A])(f: A => Option[B]): Option[B] = ma flatMap f
    }

    val fa: Option[Int] = Some(7)
    val fb: Option[Int] = Some(8)
    val fc: Option[Int] = Some(9)

    val f: Int => Option[Double] = v => M.map(M.unit(v))(_.toDouble)
    val g: Double => Option[String] = v => M.map(M.unit(v))(_.toString)

    it("map2のMonad実装において、アプリカティブ則の同一律(左単位元)を満たす。") {
      assert(
        M.map2(M.unit(()), fa)((_, a) => a) === fa
      )
    }

    it("map2のMonad実装において、アプリカティブ則の同一律(右単位元)を満たす。") {
      assert(
        M.map2(fa, M.unit(()))((a, _) => a) === fa
      )
    }
  }
}