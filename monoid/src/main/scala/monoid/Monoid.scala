package monoid

import scala.language.higherKinds

trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}

object Monoid {
  val stringMonoid = new Monoid[String] {
    def op(a1: String, a2: String): String = a1 + a2
    def zero: String = ""
  }

  val intAddition = new Monoid[Int] {
    def op(a1: Int, a2: Int): Int = a1 + a2
    val zero: Int = 0
  }

  def listMonoid[A] = new Monoid[List[A]] {
    def op(list1: List[A], list2: List[A]): List[A] = list1 ++ list2
    def zero: List[A] = Nil
  }

  def optionMonoid[A] = new Monoid[Option[A]] {
    def op(opt1: Option[A], opt2: Option[A]): Option[A] = opt1 orElse opt2
    def zero: Option[A] = None
  }

  def endoMonoid[A] = new Monoid[A => A] {
    def op(a1: A => A, a2: A => A): A => A = a1 compose a2
    def zero: A => A = identity
  }

  def identity[A](a: A): A = a

  def concatenate[A](as: List[A], m: Monoid[A]): A = as.foldLeft(m.zero)(m.op)

  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B = as.foldLeft(m.zero)((x, y) => m.op(x, f(y)))

  def foldMapV[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
    if (v.length == 0) m.zero
    else if (v.length == 1) f(v(0))
    else {
      val (l, r) = v.splitAt(v.length / 2)
      m.op(foldMapV(l, m)(f), foldMapV(r, m)(f))
    }
  }

  def productMonoid[A, B](ma: Monoid[A], mb: Monoid[B]): Monoid[(A, B)] = new Monoid[(A, B)] {
    def op(t1: (A, B), t2: (A, B)): (A, B) = (ma.op(t1._1, t2._1), mb.op(t1._2, t2._2))
    def zero: (A, B) = (ma.zero, mb.zero)
  }

  def functionMonoid[A, B](b: Monoid[B]): Monoid[A => B] = new Monoid[A => B] {
    def op(f: A => B, g: A => B): A => B = a => b.op(f(a), g(a))
    def zero: A => B = _ => b.zero
  }
}

trait Foldable[F[_]] {
  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B
  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B
  def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B
  def concatenate[A](as: F[A])(m: Monoid[A]): A = foldLeft(as)(m.zero)(m.op)
  def toList[A](fa: F[A]): List[A] = foldRight(fa)(List.empty[A])(_ :: _)
}

object FoldableList extends Foldable[List] {
  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B = as.foldRight(z)(f)
  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B = as.foldLeft(z)(f)
  def foldMap[A, B](as: List[A])(f: A => B)(mb: Monoid[B]): B = as.foldRight(mb.zero)((x, y) => mb.op(f(x), y))
  override def concatenate[A](as: List[A])(ma: Monoid[A]): A = as.foldRight(ma.zero)(ma.op)
}

object FoldableStream extends Foldable[Stream] {
  def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B): B = as.foldRight(z)(f)
  def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B): B = as.foldLeft(z)(f)
  def foldMap[A, B](as: Stream[A])(f: A => B)(mb: Monoid[B]): B = as.foldRight(mb.zero)((x, y) => mb.op(f(x), y))
  override def concatenate[A](as: Stream[A])(ma: Monoid[A]): A = as.foldRight(ma.zero)(ma.op)
}

object FoldableOption extends Foldable[Option] {
  def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B): B = as match {
    case None => z
    case Some(v) => f(v, z)
  }
  def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B): B = as match {
    case None => z
    case Some(v) => f(z, v)
  }
  def foldMap[A, B](as: Option[A])(f: A => B)(mb: Monoid[B]): B = as match {
    case None => mb.zero
    case Some(v) => f(v)
  }
}

