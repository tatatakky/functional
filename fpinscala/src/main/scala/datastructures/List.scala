package datastructures

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def tail[A](l: List[A]): List[A] = l match {
    case Nil => sys.error("tail of empty list")
    case Cons(_, tail) => tail
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => sys.error("tail of empty list")
    case Cons(_, tail) => Cons(h, tail)
  }

  def drop[A](l: List[A], n: Int): List[A] = {
    if (n > 0) {
      l match {
        case Nil => sys.error("tail of empty list")
        case Cons(_, tail) => drop(tail, n - 1)
      }
    } else l
  }

  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Cons(h, t) if f(h) => dropWhile(t)(f)
    case _ => l
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => sys.error("tail of empty list")
    case Cons(_, Nil) => Nil
    case Cons(t, h) => Cons(t, init(h))
  }

  def foldRight[A, B](as: List[A], z:B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum(l: List[Int]): Int = {
    foldRight(l, 0)((x, y) => x + y)
  }

  def product(l: List[Int]): Int = {
    foldRight(l, 1)((x,y) => x*y)
  }

  def length[A](as: List[A]): Int = foldRight(as, 0)((_, acc) => acc + 1)

  @annotation.tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z,x))(f)
    }

  def sum2(l: List[Int]): Int = foldLeft(l,0)((x, y) => x + y)

  def product2(l: List[Int]): Int = foldLeft(l, 1)(_ * _)

  def length2[A](l: List[A]): Int =
    foldLeft(l,0)((acc, _) => acc + 1)

  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, List[A]())((x, y) => Cons(y, x))

  def appendLeft[A](l: List[A], r: List[A]): List[A] =
    foldLeft(reverse(l), r)((h, t) => Cons(t, h))

  def appendRight[A](l: List[A], r: List[A]): List[A] =
    foldRight(l, r)(Cons(_, _))

  def addOne1(l: List[Int]): List[Int] = l match {
    case Nil => Nil
    case Cons(x, xs) => Cons(x+1, addOne1(xs))
  }

  def addOne2(l: List[Int]): List[Int] =
    foldRight(l, List[Int]())((h, t) => Cons(h+1, t))

  def doubleToString(l: List[Double]): List[String] =
    foldRight(l, List[String]())((h, t) => Cons(h.toString, t))

  def map[A, B](l: List[A])(f: A => B): List[B] =
    foldRight(l, Nil:List[B])((x, y) => Cons(f(x) ,y))

  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRight(as, Nil: List[A])((h, t) => if(f(h)) Cons(h, t) else t)

  def concat[A](ll: List[List[A]]): List[A] = foldRight(ll, Nil:List[A])(appendRight)

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = concat(map(as)(f))

  def addTwoList(a: List[Int], b: List[Int]): List[Int] = (a, b) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(x, xs), Cons(y, ys)) => Cons(x+y, addTwoList(xs, ys))
  }
  def withZip[A, B, C](a: List[A], b: List[B])(f: (A, B) => C): List[C] = (a, b) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(x, xs), Cons(y, ys)) => Cons(f(x, y), withZip(xs, ys)(f))
  }

  def a(l: List[Int]): List[Int] = l match {
    case Nil => a(Cons(1, Nil))
    case Cons(x, _) => Cons(x, a(Cons(x+1, Nil)))
  }

//  //末尾再帰
//  def b(l: List[Int], n: Int): List[Int] =
//    if(n==0) l
//    else b(n :: l , n-1)

}