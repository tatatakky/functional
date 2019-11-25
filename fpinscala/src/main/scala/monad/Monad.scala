package monad

trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]

  def distributed[A, B](fab: F[(A, B)]): (F[A], F[B]) =
    (map(fab)(_._1), map(fab)(_._2))

  def codistribute[A, B](e: Either[F[A], F[B]]): F[Either[A, B]] =
    e match {
      case Left(err) => map(err)(Left(_))
      case Right(value) => map(value)(Right(_))
    }
}

object Functor {

  val listFunctor = new Functor[List] {
    def map[A, B](fa: List[A])(f: A => B): List[B] = fa map f
  }

}

trait Mon[F[_]] {

  def map[A, B](fa: F[A])(f: A => B): F[B]

  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    flatMap(fa)(a => map(fb)(b => f(a, b)))

}

trait Monad[F[_]] extends Functor[F]{
  def unit[A](a: => A): F[A]

  def flatMap[A, B](ma: F[A])(f: A => F[B]): F[B]

  def map[A, B](fa: F[A])(f: A => B): F[B] =
    flatMap(fa)(a => unit(f(a)))

  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    flatMap(fa)(a => map(fb)(b => f(a, b)))

  def sequence[A](lma: List[F[A]]): F[List[A]] =
    lma.foldRight(unit(List[A]()))((x, y) => map2(x, y)(_ :: _))

  def traverse[A, B](la: List[A])(f: A => F[B]): F[List[B]] = {
    //    sequence(la.map(a => f(a)))
    la.foldRight(unit(List[B]()))((x, y) => map2(f(x), y)(_ :: _))
  }

  def replicateM[A](n: Int, ma: F[A]): F[List[A]] =
    sequence(List.fill(n)(ma))

  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => flatMap(f(a))(g)

  def join[A](mma: F[F[A]]): F[A] = flatMap(mma)(ma => ma)

}

object Monad {

  val optionMonad: Monad[Option] = new Monad[Option] {
    def unit[A](a: => A): Option[A] = Some(a)
    def flatMap[A, B](ma: Option[A])(f: A => Option[B]): Option[B] =
      ma flatMap f
  }

  val streamMonad: Monad[Stream] = new Monad[Stream] {
    def unit[A](a: => A): Stream[A] = Stream(a)
    def flatMap[A, B](ma: Stream[A])(f: A => Stream[B]): Stream[B] =
      ma flatMap f
  }

  val listMonad: Monad[List] = new Monad[List] {
    def unit[A](a: => A): List[A] = List(a)
    def flatMap[A, B](ma: List[A])(f: A => List[B]): List[B] =
      ma flatMap f
  }

  def stateMonad[S] = new Monad[({type f[x] = State[S, x]})#f] {
    def unit[A](a: => A): State[S, A] = State(s => (a, s))
    def flatMap[A, B](sa: State[S, A])(f: A => State[S, B]): State[S, B] =
      sa flatMap f
  }

}

case class State[S, +A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    State(s => {
      val (a1, s1) = run(s)
      (f(a1), s1)
    })

  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State(s => {
      val (a1, s1) = run(s)
      f(a1).run(s1)
    })
}

object State {

  import Monad.stateMonad

  def getState[S]: State[S, S] = State(s => (s, s))

  def setState[S](s: => S): State[S, Unit] = State(_ => ((), s))

  val F = stateMonad[Int]

  def zipWithIndex[A](as: List[A]): List[(Int, A)] =
    as.foldLeft(F.unit(List[(Int, A)]()))((acc, a) =>
      for {
        xs <- acc
        n <- getState
        _ <- setState(n + 1)
      } yield
        (n, a) :: xs)
      .run(0)
      ._1
      .reverse

}


case class Id[A](value: A) {
  def map[B](f: A => B): Id[B] = Id(f(value))
  def flatMap[B](f: A => Id[B]): Id[B] = f(value)
}

object StateMonadProject {

  type IntState[A] = State[Int, A]

  object IntStateMonad extends Monad[IntState] {
    def unit[A](a: => A): IntState[A] = State(s => (a, s))
    def flatMap[A, B](intState: IntState[A])(f: A => IntState[B]): IntState[B] =
      intState flatMap f
  }

}