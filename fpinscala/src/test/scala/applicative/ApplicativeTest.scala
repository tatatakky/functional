package applicative

import org.scalatest.FunSpec

class ApplicativeTest extends FunSpec {

  describe("Applicative") {

    it("review") {
      def curry[A, B, C](f: (A, B) => C): A => B => C =
        a => b => f(a, b)

      assert(curry[Int, Int, Int]((x, y) => x + y)(10)(20) === 30)

      // create (Int, Int, Int, Int) => Int = <function4>
      def add(a: Int, b: Int, c: Int, d: Int): Int = a + b + c + d

      val curriedAdd: Int => Int => Int => Int => Int = (add _).curried
      assert(curriedAdd(10)(20)(30)(40) === 100)
    }

    it("Applicative trait") {

      val applicative = new Applicative[Option] {

        override def apply[A, B](fab: Option[A => B])(fa: Option[A]): Option[B] =
          (fab, fa) match {
            case (Some(ab), Some(a)) => Some(ab(a))
            case _ => None
          }

        def unit[A](a: => A): Option[A] = Some(a)
      }

      val applicativez = new Applicative[Option] {

        override def map2[A, B, C](fa: Option[A], fb: Option[B])(f: (A, B) => C): Option[C] =
          (fa, fb) match {
            case (Some(a), Some(b)) => Some(f(a, b))
            case _ => None
          }

        def unit[A](a: => A): Option[A] = Some(a)
      }

      // applicativeとapplicativezにおいて、map2の表現力が同等であることの証明。
      assert(
        applicative.map2(Some(1), Some(2))(_ + _) ===
          applicativez.map2(Some(1), Some(2))(_ + _)
      )

      // applicativeとapplicativezにおいて、mapの表現力が同等であることの証明。
      assert(
        applicative.map(Some(1))(_ * 2) ===
          applicativez.map(Some(1))(_ * 2)
      )

      // map3 can work as expected.
      assert(
        applicative.map3(Some(1), Some(2), Some(3))(_ * _ * _) ===
          Some(6)
      )

      // map4 can work as expected.
      assert(
        applicative.map4(Some(1), Some(2), Some(3), Some(4))(_ + _ + _ + _) ===
          Some(10)
      )
    }
  }

  describe("Applicative Functor but not Monad") {

    it("Stream Applicative") {

      val streamList = List(Stream(1, 2), Stream(3), Stream(4, 5, 6))

      //有限とは限らないStream
      val streamApplicativez = new Applicative[Stream] {

        //無限の定数Stream
        def unit[A](a: => A): Stream[A] = Stream.continually(a)

        //要素を各箇所で結合する関数map2
        override def map2[A, B, C](sa: Stream[A], sb: Stream[B])(f: (A, B) => C): Stream[C] =
          sa.zip(sb).map(ab => f.tupled(ab))
      }
      val strmApplicativeSequence = streamApplicativez.sequence(streamList)
      assert(strmApplicativeSequence === Stream(List(1, 3, 4)))
    }

    it("Either monad") {

      import monad.Monad._

      def eitherMonad[E]: Monad[({type f[x] = Either[E, x]})#f] =
        new Monad[({type f[x] = Either[E, x]})#f] {
          def unit[A](a: => A): Either[E, A] = Right(a)

          def flatMap[A, B](ea: Either[E, A])(f: A => Either[E, B]) = ea match {
            case Left(err) => Left(err)
            case Right(v) => f(v)
          }
        }
    }

    it("Validation Applicative") {

      import java.util.Date

      case class WebForm(name: String, birthDate: String, phoneNumber: String)

      def validName(name: String): Validation[String, String] =
        if (name != "") Success(name)
        else Failure("Name cannot be empty")

      def validBirthDate(birthday: String): Validation[String, String] = {
        try {
          import java.text._
          Success(birthday)
        } catch {
          case _: Exception => Failure("BirthDate must be in the form yyyy-MM-dd")
        }
      }

      def validPhone(phoneNumber: String): Validation[String, String] =
        if (phoneNumber.matches("[0-9]{10}")) Success(phoneNumber)
        else Failure("Phone number must be 10 digits")

      def validWebForm(name: String, birthdate: String, phone: String): Validation[String, WebForm] =
        validationApplicative.map3(validName(name), validBirthDate(birthdate), validPhone(phone))(WebForm)

      def validationApplicative[E]: Applicative[({type f[x] = Validation[E, x]})#f] =
        new Applicative[({type f[x] = Validation[E, x]})#f] {
          def unit[A](a: => A): Validation[Nothing, A] = Success(a)

          override def map2[A, B, C](va: Validation[E, A], vb: Validation[E, B])(f: (A, B) => C): Validation[E, C] =
            (va, vb) match {
              case (Success(a), Success(b)) => Success(f(a, b))
              case (Failure(h1, t1), Failure(h2, t2)) => Failure(h1, t1 ++ Vector(h2) ++ t2)
              case (fail@Failure(_, _), _) => fail
              case (_, fail@Failure(_, _)) => fail
            }
        }

      assert(
        validWebForm("foo", "2019/09/01", "1234567891")
          === Success(WebForm("foo", "2019/09/01", "1234567891"))
      )

      assert(
        validWebForm("", "2019/09/01", "135")
          === Failure("Name cannot be empty", Vector("Phone number must be 10 digits"))
      )
    }
  }

}