package state

trait RNG {
  def nextInt: (Int, RNG)
}

object RNG {

  case class SimpleRNG(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = SimpleRNG(newSeed)
      val n = (newSeed >>> 16).toInt // Long => Int
      (n, nextRNG)
    }
  }

  def randomPair(rng: RNG): ((Int, Int), RNG) = {
    val (i1, rng2) = rng.nextInt
    val (i2, rng3) = rng2.nextInt
    ((i1, i2), rng3)
  }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i1, rng2) = rng.nextInt
    (if (i1 < 0) -(i1 + 1) else i1, rng2)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (i, r) = nonNegativeInt(rng)
    (i / (Int.MaxValue.toDouble + 1), r)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (int, r) = rng.nextInt
    val (dbl, r2) = double(r)
    ((int, dbl), r2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((int, dbl), r) = intDouble(rng)
    ((dbl, int), r)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (dbl, r2) = double(rng)
    val (dbl2, r3) = double(r2)
    val (dbl3, r4) = double(r3)
    ((dbl, dbl2, dbl3), r4)
  }

  def boolean(rng: RNG): (Boolean, RNG) =
    rng.nextInt match {
      case (i, rng2) => (i % 2 == 0, rng2)
    }


  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    @annotation.tailrec
    def go(c: Int)(as: List[Int], r: RNG): (List[Int], RNG) = {
      if(c == 0) (as, r)
      else {
        val (n, r2) = r.nextInt
        go(c-1)(n :: as, r2)
      }
    }
    go(count)(List(), rng)
  }

  //State Action
  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

//  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
//    rng => {
//      println(s"rng : $rng")
//      val (a, rng2) = s(rng)
//      println(s"rng2: $rng2")
//      (f(a), rng2)
//    }

  def _double(rng: RNG): Rand[Double] = {
    map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))
  }

//  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
//    rng => {
//      val (a, rng2) = ra(rng)
//      val (b, rng3) = rb(rng2)
//      (f(a, b), rng3)
//    }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    map2(ra, rb)((_, _))

  val randIntDouble: Rand[(Int, Double)] = both(int, double)
  val randDoubleInt: Rand[(Double, Int)] = both(double, int)

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    fs.foldRight(unit(List[A]()))((x, y) => map2(x, y)(_ :: _))
  }

  def intsRND(count: Int)(rng: RNG): Rand[List[Int]] =
    sequence(List.fill(count)(int))

//  def nonNegativeLessThan(n: Int): Rand[Int] = map(nonNegative)(_ % n)

//  def nonNegativeLessThan(n: Int): Rand[Int] =
//    map(nonNegativeInt){ i =>
//      val mod = i % n
//      if(i + (n-1) - mod >= 0) (mod, ) else nonNegativeLessThan(n)(???)
//    }

  //TODO: (RNG => (Int, RNG)) => (Int, RNG)に型変換する方法？
//  def nonNegativeLessThan(n: Int): Rand[Int] =
//    rng => {
//      val (i, rng2) = nonNegativeInt(rng)
//      val mod = i % n
//      if(i + (n-1) - mod >= 0) (mod, rng2)
//      else nonNegativeLessThan(n)(rng)
//    }


  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (i, rng2) = f(rng)
      g(i)(rng2)
    }


  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(x => unit(f(x)))

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => map(rb)(b => f(a, b)))

  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(rng => nonNegativeInt(rng)){ i =>
      val mod = i % n
      if(i + (n-1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
    }

  //In the case of following, output is 0 to 5
//  def rollDie: Rand[Int] = nonNegativeLessThan(6)

  //In the case of following, output is 1 to 6
  def rollDie: Rand[Int] = map(nonNegativeLessThan(6))(_ + 1)
}

import State._

case class State[S, +A](run: S => (A, S)){

  def map[B](f: A => B): State[S, B] =
    flatMap(a => unit(f(a)))

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => sb.map(b => f(a, b)))

  def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
    val (a, s1) = run(s)
    f(a).run(s1)
  })
}

object State{

//  type Rand[+A] = RNG => (A, RNG)
//  type State[S, +A] = S => (A, S)

  def unit[S, A](a: A): State[S, A] = State(s => (a, s))

  def sequence[S, A](ls: List[State[S, A]]): State[S, List[A]] =
    ls.foldRight(unit[S, List[A]](List()))((x, y) => x.map2(y)(_ :: _))

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()

}


sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coin: Int) //State

object Candies {

//  val state: Machine = Machine(true, 5, 10)

  def operate = (i: Input) => (s: Machine) =>
    (i, s) match {
      case (_, Machine(_, 0, _)) => s
      case (Coin, Machine(false, _, _)) => s
      case (Turn, Machine(true, _, _)) => s
      case (Coin, Machine(true, candies, coin)) => Machine(false, candies, coin+1)
      case (Turn, Machine(false, candies, coin)) => Machine(true, candies-1, coin)
    }

  //State[Machine, (Int, Int)] = Machine => ((Int, Int), Machine)
//  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
}