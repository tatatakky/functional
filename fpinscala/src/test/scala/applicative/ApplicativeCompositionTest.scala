package applicative

import org.scalatest.AsyncFlatSpec

class ApplicativeCompositionTest extends AsyncFlatSpec {

  "Applicative compose function" should
    "be able to compose Option Applicative and Future Applicative." in {

    // Future Applicative
    import scala.concurrent.Future
    val F: Applicative[Future] = new Applicative[Future] {
      def unit[A](a: => A): Future[A] = Future(a)
      override def map2[A, B, C](fa: Future[A], fb: Future[B])(f: (A, B) => C): Future[C] =
        for {
          a <- fa
          b <- fb
        } yield f(a, b)
    }

    // Option Applicative
    val G: Applicative[Option] = new Applicative[Option] {
      def unit[A](a: => A): Option[A] = Some(a)
      override def map2[A, B, C](fa: Option[A], fb: Option[B])(f: (A, B) => C): Option[C] =
        (fa, fb) match {
          case (Some(a), Some(b)) => Some(f(a, b))
          case _                  => None
        }
    }

    val x: Future[Option[Int]] = Future(Some(5))
    val y: Future[Option[Int]] = Future(Some(10))

    F.compose(G).map2(x, y)(_ + _).map(sm => assert(sm.contains(15)))
  }
}
