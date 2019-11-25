package applicative

import scala.language.higherKinds

import monad.{Functor}
import monoids._
import state._

trait Applicative[F[_]] extends Functor[F] {

  def apply[A, B](fab: F[A => B])(fa: F[A]): F[B] =
    map2(fab, fa)((ab, a) => ab(a))

  def unit[A](a: => A): F[A]

  def map[A, B](fa: F[A])(f: A => B): F[B] =
    apply(unit(f))(fa)

  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    apply(map(fa)(f.curried))(fb)

  def map3[A, B, C, D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] =
    apply(apply(apply(unit(f.curried))(fa))(fb))(fc)

  def map4[A, B, C, D, E](fa: F[A], fb: F[B], fc: F[C], fd: F[D])(f: (A, B, C, D) => E): F[E] =
    apply(apply(apply(apply(unit(f.curried))(fa))(fb))(fc))(fd)

  def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] =
    as.foldRight(unit(List[B]()))((x, y) => map2(f(x), y)(_ :: _))

  def sequence[A](fas: List[F[A]]): F[List[A]] =
    traverse(fas)(fa => fa)

  def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] = map2(fa, fb)((_, _))

  def assoc[A, B, C](p: (A, (B, C))): ((A, B), C) =
    p match {case (a, (b, c)) => ((a, b), c)}

  def productF[I, O, I2, O2](f: I => O, g: I2 => O2): (I, I2) => (O, O2) =
    (i, i2) => (f(i), g(i2))

  def _product[G[_]](G: Applicative[G]): Applicative[({type f[x] = (F[x], G[x])})#f] = {
    val self = this
    new Applicative[({type f[x] = (F[x], G[x])})#f] {
      def unit[A](a: => A): (F[A], G[A]) = (self.unit(a), G.unit(a))
      override def apply[A, B](fgab: (F[A => B], G[A => B]))(fga: (F[A], G[A])): (F[B], G[B]) =
        (self.apply(fgab._1)(fga._1), G.apply(fgab._2)(fga._2))
    }
  }

  def compose[G[_]](G: Applicative[G]): Applicative[({type f[x] = F[G[x]]})#f] = {
    val self = this
    new Applicative[({type f[x] = F[G[x]]})#f] {
      def unit[A](a: => A): F[G[A]] = self.unit(G.unit(a))
      override def map2[A, B, C](fga: F[G[A]], fgb: F[G[B]])(f: (A, B) => C): F[G[C]] =
        self.map2(fga, fgb)(G.map2(_, _)(f))
    }
  }

  def sequenceMap[K, V](ofa: Map[K, F[V]]): F[Map[K, V]] =
    ofa.foldRight(unit(Map[K, V]()))((kfv, fb) => map2(fb, kfv._2)((b, v) => b + (kfv._1 -> v)))

}

object Applicative {

  import monoids.Monoid
  type Const[M, B] = M
  def monoidApplicative[M](m: Monoid[M]): Applicative[({type f[x] = Const[M, x]})#f] =
    new Applicative[({type f[x] = Const[M, x]})#f] {
      def unit[A](a: => A): M = m.zero
      override def map2[A, B, C](m1: M, m2: M)(f: (A, B) => C): M = m.op(m1, m2)
    }

}

trait Monad[F[_]] extends Applicative[F] {
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

  def join[A](ffa: F[F[A]]): F[A] = flatMap(ffa)(fa => fa)

  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => flatMap(f(a))(g)

  override def map[A, B](fa: F[A])(f: A => B): F[B] =
    flatMap(fa)(a => unit(f(a)))

  override def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    flatMap(fa)(a => map(fb)(b => f(a, b)))
}

object Monad {
  def stateMonad[S] = new Monad[({type f[x] = State[S, x]})#f] {
    def unit[A](a: => A): State[S, A] = State(s => (a, s))
    def flatMap[A, B](sa: State[S, A])(f: A => State[S, B]): State[S, B] =
      sa flatMap f
  }
}

sealed trait Validation[+E, +A]

case class Failure[E](head: E, tail: Vector[E] = Vector()) extends Validation[E, Nothing]

case class Success[A](a: A) extends Validation[Nothing, A]

import monoids.Foldable

trait Traverse[F[_]] extends Functor[F] with Foldable[F] {

  def traverse[G[_]: Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]] =
    sequence(map(fa)(f))

  def sequence[G[_]: Applicative, A](fga: F[G[A]]): G[F[A]] =
    traverse(fga)(ga => ga)

  type Id[A] = A

  val idMonad = new Monad[Id] {
    def unit[A](a: => A): Id[A] = a
    def flatMap[A, B](ia: Id[A])(f: A => Id[B]): Id[B] = f(ia)
  }

  def map[A, B](fa: F[A])(f: A => B): F[B] = traverse[Id, A, B](fa)(f)(idMonad)

  import Applicative._
  override def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
    traverse[({type f[x] = Const[B, x]})#f, A, Nothing](as)(f)(monoidApplicative(mb))

  def traverseS[S, A, B](fa: F[A])(f: A => State[S, B]): State[S, F[B]] =
    traverse[({type f[x] = State[S, x]})#f, A, B](fa)(f)(Monad.stateMonad)
}

object Traverse {
  val listTraverse = new Traverse[List] {
    override def traverse[M[_], A, B](fa: List[A])(f: A => M[B])(implicit M: Applicative[M]): M[List[B]] =
      fa.foldRight(M.unit(List[B]()))((a, mlb) => M.map2(f(a), mlb)(_ :: _))
  }

  val optionTraverse = new Traverse[Option] {
    override def traverse[M[_], A, B](fa: Option[A])(f: A => M[B])(implicit M: Applicative[M]): M[Option[B]] =
      fa match {
        case Some(value) => M.map(f(value))(Some(_))
        case None => M.unit(None)
      }
  }
}