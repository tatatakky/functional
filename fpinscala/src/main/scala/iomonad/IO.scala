package iomonad

import scala.io.StdIn.readLine

object IOFirstStep {
  trait IO { self =>
    def run: Unit
    def ++(io: IO): IO = new IO {
      def run: Unit = {
        self.run
        io.run
      }
    }
  }

  object IO {
    def empty: IO = new IO { def run = () }
  }

  def fahrenheitToCelsius(f: Double): Double = (f - 32) * 5.0 / 9.0

  // This is the code with side effect (副作用のあるコード)
  def converter: Unit = {
    println("Enter a temperature in degrees Fahrenheit: ")
    val d = readLine().toDouble
    println(fahrenheitToCelsius(d))
  }
}

object IOSecondStep {

  trait IO[A] { self =>
    def run: A

    def map[B](f: A => B): IO[B] = new IO[B] {
      def run: B = f(self.run)
    }

    def flatMap[B](f: A => IO[B]): IO[B] = new IO[B] {
      def run: B = f(self.run).run
    }
  }

  object IO extends Monad[IO] {
    def pure[A](a: => A): IO[A] = new IO[A] { def run: A = a}
    def flatMap[A, B](fa: IO[A])(f: A => IO[B]): IO[B] = fa flatMap f
    def apply[A](a: => A): IO[A] = pure(a)
  }
}

//object IOThirdStep {
//
//  case class Return[A](a: A) extends IO[A]
//  case class Suspend[A](resume: () => A) extends IO[A]
//  case class FlatMap[A,B](sub: IO[A], k: A => IO[B]) extends IO[B]
//
//  trait IO[A] {
//     IO[A]型の値に応じて、fを適用するか、自分自身を返すかを決めるFlatMap
//    def flatMap[B](f: A => IO[B]) = FlatMap(this, f)
//    def map[B](f: A => B): IO[B] = flatMap(f andThen (Return(_)))
//  }
//
//  def printLine(s: String): IO[Unit] =
//    Suspend(() => println(s))
//
//  val printer: IO[Unit] = IO.forever(printLine("Still going ..."))
//
//  object IO extends Monad[IO] {
//    def pure[A](a: => A): IO[A] = Return(a)
//    def flatMap[A, B](fa: IO[A])(f: A => IO[B]): IO[B] =
//      fa flatMap f
//  }
//
//  @annotation.tailrec
//  def run[A](io: IO[A]): A = io match {
//    case Return(a) => a
//    case Suspend(r) => r()
//    case FlatMap(x, f) => x match {
//      case Return(a) => run(f(a))
//      case Suspend(r) => run(f(r()))
//    }
//  }
//
//}

object Test {

  import iomonad.IOSecondStep.IO
  import iomonad.IOSecondStep.IO._
  import iomonad.IOFirstStep.fahrenheitToCelsius
//  import iomonad.IOThirdStep.printLine

  def ReadLine: IO[String] = IO { readLine() }
  def PrintLine(msg: String): IO[Unit] = IO { println(msg) }

  def converter: IO[Unit] =
    for {
      _ <- PrintLine("Enter a temperature in degrees Fahrenheit: ")
      d <- ReadLine.map(_.toDouble)
      _ <- PrintLine(fahrenheitToCelsius(d).toString)
    } yield ()

  val echo = ReadLine.flatMap(PrintLine)

  val readInt = ReadLine.map(_.toInt)

  val readInts = readInt ** readInt

  val readList = replicateM(10)(ReadLine)

  def readIntConverter: IO[Unit] =
    for {
      in <- readInt
      _ <- PrintLine(s"Input: $in")
    } yield ()

  def readIntsConverter: IO[Unit] =
    for {
      in <- readInts
      _ <- PrintLine(s"Input two data: $in")
    } yield ()

  def readListConverter: IO[Unit] =
    for {
      lists <- readList
      _ <- PrintLine(s"Input list data : $lists")
    } yield ()

  val p = IO.forever(PrintLine("Still going ..."))

}

object Test3 {

//  import iomonad.IOThirdStep._
//
//  val printer = IO.forever(printLine("Still going ..."))

}