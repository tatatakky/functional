package monoids

trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A

}

object Monoid {

  val stringMonoid = new Monoid[String] {
    def op(a1: String, a2: String): String = a1 + a2
    def zero: String = ""
  }

  def listMonoid[A] = new Monoid[List[A]] {
    def op(a1: List[A], a2: List[A]): List[A] = a1 ++ a2
    def zero: List[A] = Nil
  }

  val intAddition: Monoid[Int] = new Monoid[Int] {
    def op(x: Int, y: Int): Int = x + y
    def zero: Int = 0
  }

  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    def op(x: Int, y: Int): Int = x * y
    def zero: Int = 1
  }

  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    def op(x: Boolean, y: Boolean): Boolean = x || y
    def zero: Boolean = false
  }

  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    def op(x: Boolean, y: Boolean): Boolean = x && y
    def zero: Boolean = true
  }

  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    def op(x: Option[A], y: Option[A]): Option[A] = x orElse y
    def zero: Option[A] = None
  }

  def dual[A](m: Monoid[A]): Monoid[A] = new Monoid[A] {
    def op(x: A, y: A): A = m.op(y, x)
    val zero = m.zero
  }

  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A]{
    def op(x: A => A, y: A => A): A => A = x compose y
    def zero: A => A = (a: A) => a
  }

  def concatenate[A](as: List[A], m: Monoid[A]): A =
    as.foldLeft(m.zero)(m.op)

  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    as.map(f).foldLeft(m.zero)(m.op)

  def foldMapV[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
    if(v.isEmpty) {
//      println("length : 0")
      m.zero
    } else if (v.length == 1){
//      println("length : 1")
      f(v(0))
    } else {
      val (l, r) = v.splitAt(v.length / 2)
      println("l : " + l)
      println("r : " + r)
      m.op(foldMapV(l, m)(f), foldMapV(r, m)(f))
    }
  }


  def productMonoid[A, B](A: Monoid[A], B: Monoid[B]): Monoid[(A, B)] = new Monoid[(A, B)] {
    def zero: (A, B) = (A.zero, B.zero)
    def op(x: (A, B), y: (A, B)): (A, B) =
      (A.op(x._1, y._1), B.op(x._2, y._2))
  }


  def mapMergeMonoid[K, V](MV: Monoid[V]): Monoid[Map[K, V]] = new Monoid[Map[K, V]] {

    def zero: Map[K, V] = Map[K, V]()
    def op(a: Map[K, V], b: Map[K, V]): Map[K, V] = {
      (a.keySet ++ b.keySet).foldRight(zero){ (k, acc) =>
        acc.updated(k, MV.op(a.getOrElse(k, MV.zero),
          b.getOrElse(k, MV.zero)))
      }
    }

  }


  //wordCount
  sealed trait WC
  case class Stub(chars: String) extends WC
  case class Part(lStub: String, words: Int, rStub: String) extends WC

  val wcMonoid: Monoid[WC] = new Monoid[WC] {
    def zero: Stub = Stub("")
    def op(l: WC, r: WC): WC = (l, r) match {
      case (Stub(a), Stub(b)) => Stub(a + b)
      case (Stub(c), Part(lStub, words, rStub)) => Part(c + lStub, words, rStub)
      case (Part(lStub, words, rStub), Stub(c)) => Part(lStub, words, rStub + c)
      case (Part(lStub1, words1, rStub1), Part(lStub2, words2, rStub2)) =>
        Part(lStub1, words1 + (if((rStub1 + lStub2).isEmpty) 0 else 1) + words2, rStub2)
    }
  }

  def wordCount(s: String): Int = {
    def wc(c: Char): WC = {
      if (c.isWhitespace) {
        Part("", 0, "")
      } else {
        Stub(c.toString)
      }
    }
    def judgeGoodWord(s: String): Int = {
      if(s.isEmpty) 0
      else 1
    }
    foldMapV(s.toIndexedSeq, wcMonoid)(wc) match {
      case Stub(c) => judgeGoodWord(c)
      case Part(lStub, words, rStub) =>
        judgeGoodWord(lStub) + words + judgeGoodWord(rStub)
    }
  }
}

trait Foldable[F[_]] {

  import Monoid.endoMonoid
  import Monoid._

  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B =
    foldMap(as)(f.curried)(endoMonoid[B])(z)

  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B =
    foldMap(as)(a => (b: B) => f(b, a))(endoMonoid[B])(z)

  def foldMap[A, B](as: F[A])(f: A => B)(m: Monoid[B]): B =
    foldLeft(as)(m.zero)((x, y) => m.op(x, f(y)))

  def concatenate[A](as: F[A])(m: Monoid[A]): A =
    foldLeft(as)(m.zero)(m.op)

}

object ListFoldable extends Foldable[List] {

  override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
    as.foldRight(z)(f)

  override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
    as.foldLeft(z)(f)

  override def foldMap[A, B](as: List[A])(f: A => B)(m: Monoid[B]): B =
    foldLeft(as)(m.zero)((x, y) => m.op(x, f(y)))

  override def concatenate[A](as: List[A])(m: Monoid[A]): A =
    as.foldRight(m.zero)(m.op)
}

object IndexedSeqFoldable extends Foldable[IndexedSeq] {

  override def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B): B =
    as.foldRight(z)(f)

  override def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B): B =
    as.foldLeft(z)(f)

  override def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(m: Monoid[B]): B =
    foldLeft(as)(m.zero)((x, y) => m.op(x, f(y)))

  override def concatenate[A](as: IndexedSeq[A])(m: Monoid[A]): A =
    as.foldLeft(m.zero)(m.op)
}

object StreamFoldable extends Foldable[Stream] {

  override def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B): B =
    as.foldRight(z)(f)

  override def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B): B =
    as.foldLeft(z)(f)

}

object OptionFoldable extends Foldable[Option] {

  override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B): B =
    as match {
      case None => z
      case Some(a) => f(a, z)
    }

  override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B): B =
    as match {
      case None => z
      case Some(a) => f(z, a)
    }

  override def foldMap[A, B](as: Option[A])(f: A => B)(m: Monoid[B]): B =
    as match {
      case None => m.zero
      case Some(a) => f(a)
    }

}