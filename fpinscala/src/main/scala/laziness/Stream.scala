package laziness

trait Stream[+A] {

  import Stream._

//  def headOption: Option[A] = this match {
//    case Cons(h, t) => Some(h())
//    case Empty => None
//  }

  def toList: List[A] = {
    @annotation.tailrec
    def go(s: Stream[A], xs: List[A]): List[A] = s match {
      case Cons(h, t) => go(t(), h() :: xs)
      case _          => xs
    }
    go(this, List()).reverse
  }

//  def map[B](f: A => B): Stream[B] = this match {
//    case Cons(head, tail) => Cons(() => f(head()), () => tail().map(f))
//    case _ => Empty
//  }

//  def take(n: Int): Stream[A] = this match {
//    case Cons(head, tail) if n > 1 => cons(head(), tail().take(n - 1))
//    case Cons(head, _) if n == 1   => cons(head(), empty)
//    case _                         => empty
//  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(head, tail) if n > 1  => tail().drop(n - 1)
    case Cons(head, tail) if n == 1 => tail()
    case _                          => empty
  }

//  def exists(f: A => Boolean): Boolean = this match {
//    case Cons(head, tail) => f(head()) || tail().exists(f)
//    case _ => false
//  }

//  def forAll(f: A => Boolean): Boolean = this match {
//    case Cons(head, tail) => println(head()); f(head()) && tail().forAll(f)
//    case _ => true
//  }

//    def filter(f: A => Boolean): Stream[A] = this match {
//      case Cons(head, tail) => if(f(head())) Cons[A](() => head(), () => tail().filter(f)) else tail().filter(f)
//      case _ => Empty
//    }

  //implementation for using foldRight function
  def exists(f: A => Boolean): Boolean = foldRight(false)((a, b) => f(a) || b)

  def forAll(f: A => Boolean): Boolean = foldRight(true)((a, b) => f(a) && b)

//  def takeWhile(f: A => Boolean): Stream[A] =
//    foldRight(Stream[A]())((a, b) =>
//      if (f(a)) cons(a, b.takeWhile(f)) else empty)

  def headOption: Option[A] = foldRight(None: Option[A])((h, _) => Some(h))

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(head, tail) => f(head(), tail().foldRight(z)(f))
    case _                => z
  }

  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((h, t) => cons(f(h), t))

  def filter(f: A => Boolean): Stream[A] =
    foldRight(empty[A])((h, t) =>
      if (f(h)) cons(h, t.filter(f)) else t.filter(f))

  def append[B >: A](s: => Stream[B]): Stream[B] =
    foldRight(s)((h, t) => cons(h, t))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((h, t) => f(h).append(t))

  def find(f: A => Boolean): Option[A] = filter(f).headOption

  import Stream._

  def mapUnfold[B](g: A => B): Stream[B] =
    unfold(this) {
      case Cons(head, tail) => Some(g(head()), tail())
      case _                => None
    }

  def take(n: Int): Stream[A] =
    unfold((this, n)) {
      case (Cons(head, tail), 1) => Some(head(), (empty, 0))
      case (Cons(head, tail), n) => Some(head(), (tail(), n - 1))
      case _                     => None
    }

  def takeWhile(g: A => Boolean): Stream[A] =
    unfold(this) {
      case Cons(head, tail) if g(head()) => Some(head(), tail())
      case _                             => None
    }

  def zipWith[B, C](s: Stream[B])(f: (A, B) => C): Stream[C] =
    unfold((this, s)) {
      case (Cons(head, tail), Cons(head2, tail2)) =>
        Some(f(head(), head2()), (tail(), tail2()))
      case _ => None
    }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] =
    unfold((this, s2)) {
      case (Empty, Empty) => None
      case (Cons(head, tail), Empty) =>
        Some((Some(head()), None), (tail(), empty))
      case (Empty, Cons(head2, tail2)) =>
        Some((None, Some(head2())), (empty, tail2()))
      case (Cons(head, tail), Cons(head2, tail2)) =>
        Some((Some(head()), Some(head2())), (tail(), tail2()))
    }

//  def startsWith[A](s: Stream[A]): Boolean =
//    this.zipAll(s).takeWhile(!_._2.isEmpty).forAll(e => e._1 == e._2)
def startsWith[A](s: Stream[A]): Boolean =
  this.zipAll(s).takeWhile(!_._2.isEmpty).forAll{
    case (h, t) => h == t
  }

  def tails: Stream[Stream[A]] =
    unfold(this) {
      case Empty => None
      case s     => Some((s, s.drop(1)))
    }.append(Empty)

  def hasSubSequence[A](s: Stream[A]): Boolean =
    tails exists (_ startsWith s)

//  def scanRight[B](z: B)(f: (A, B) => B): Stream[B] =
//    tails.map(e => e.foldRight(z)((x, y) => f(x, y)))

  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =
    foldRight((z, Stream(z))){(a, p0) =>
      lazy val p1 = p0
      val b2 = f(a, p1._1)
      (b2, cons(b2, p1._2))
    }._2


  //unfold function is actually in Object Stream.
//  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
//    case Some((head, tail)) => cons(head, unfold(tail)(f))
//    case None               => empty
//  }

//
//  def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])] =
//    zipWithAll(s2)((_,_))
//
//  def zipWithAll[B, C](s2: Stream[B])(f: (Option[A], Option[B]) => C): Stream[C] =
//    Stream.unfold((this, s2)) {
//      case (Empty, Empty) => None
//      case (Cons(h, t), Empty) => Some(f(Some(h()), Option.empty[B]) -> (t(), empty[B]))
//      case (Empty, Cons(h, t)) => Some(f(Option.empty[A], Some(h())) -> (empty[A] -> t()))
//      case (Cons(h1, t1), Cons(h2, t2)) => Some(f(Some(h1()), Some(h2())) -> (t1() -> t2()))
//    }

}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {

  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd //hdの評価が最初に参照される時まで先送り
    lazy val tail = tl //hlの評価が最初に参照される時まで先送り
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  //onTrue、onFalseはそれらが関数内で呼ばれた時にしか評価されない。
  def if2[A](condition: Boolean, onTrue: => A, onFalse: => A): A =
    if (condition) onTrue else onFalse

  def maybeTwice(b: Boolean, i: => Int) = {
    lazy val j = i
    if (b) j + j else 0
  }

//  val ones: Stream[Int] = cons(1, ones)

  //  def constant[A](a: A): Stream[A] = {
  ////    cons(a, constant(a))
  //    lazy val tail: Stream[A] = Cons(() => a, () => tail)
  //    tail
  //  }
  //
  //  def from(n: Int): Stream[Int] = cons(n, from(n+1))
  //
  ////  val fibs: Stream[Int] = {
  ////    def loop(first: Int, second: Int): Stream[Int] = {
  ////      val next: Int = first + second
  ////      cons(next, loop(second, next))
  ////    }
  ////    def go(first: Int, second: Int): Stream[Int] = {
  ////      cons(first,cons(second, loop(first, second)))
  ////    }
  ////    go(0, 1)
  ////  }
  //
  //  val fibs: Stream[Int] = {
  //    def go(first: Int, second: Int): Stream[Int] = {
  //      cons(first, go(second, first + second))
  //    }
  //    go(0, 1)
  //  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((head, tail)) => cons(head, unfold(tail)(f))
    case None               => empty
  }

  val ones: Stream[Int] = unfold(1)(_ => Some(1, 1))

  def constant[A](a: A): Stream[A] = unfold(a)(_ => Some(a, a))

  def from(n: Int): Stream[Int] = unfold(n)(e => Some(e, e + 1))
//
//  val fibs: Stream[Int] = {
//    unfold((0,1))(e => Some(e._1, (e._2, e._1 + e._2)))
//  }

  val fibs: Stream[Int] = {
    unfold((0, 1)) { case (head, tail) => Some(head, (tail, head + tail)) }
  }

  def mapUnfoldX[T, U](strm: Stream[T])(g: T => U): Stream[U] =
    unfold(strm) {
      case Cons(head, tail) => Some(g(head()), tail())
      case _                => None
    }

}
