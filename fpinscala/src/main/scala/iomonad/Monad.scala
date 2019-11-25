package iomonad

trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]
}

trait Monad[F[_]] extends Functor[F] {

  def pure[A](a: => A): F[A]

  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

  def map[A, B](fa: F[A])(f: A => B): F[B] =
    flatMap(fa)(a => pure(f(a)))

  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    flatMap(fa)(a => map(fb)(b => f(a, b)))

  def sequence[A](lma: List[F[A]]): F[List[A]] =
    lma.foldRight(pure(List[A]()))((x, y) => map2(x, y)(_ :: _))

  def traverse[A, B](la: List[A])(f: A => F[B]): F[List[B]] =
    la.foldRight(pure(List[B]()))((x, y) => map2(f(x), y)(_ :: _))

  def replicateM[A](n: Int)(fa: F[A]): F[List[A]] =
    sequence(List.fill(n)(fa))

  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => flatMap(f(a))(g)

  // if true, loop "doWhile" function, else return pure(())
  def doWhile[A](fa: F[A])(cond: A => F[Boolean]): F[Unit] =
    for {
      a <- fa
      ok <- cond(a) // F[Boolean] F[True] or F[false]
      _ <- if(ok) doWhile(fa)(cond) else pure(())
    } yield ()

  // lazy evaluation
  // 一度評価した値に永遠にforever関数を適用させる。
  def forever[A, B](fa: F[A]): F[B] = {
    lazy val t: F[B] = forever(fa)
    fa flatMap (_ => t)
  }

  implicit def monadic[A](fa: F[A]): Monadic[F, A] = new Monadic[F, A]{
    val F: Monad[F] = Monad.this
    def get: F[A] = fa
  }


}

trait Monadic[F[_], A] {
  val F: Monad[F]
  import F._

  def get: F[A]
  private val fa = get
  def map[B](f: A => B): F[B] = F.map(fa)(f)
  def flatMap[B](f: A => F[B]): F[B] = F.flatMap(fa)(f)
  def **[B](fb: F[B]): F[(A, B)] = map2(fa, fb)((_, _))
}