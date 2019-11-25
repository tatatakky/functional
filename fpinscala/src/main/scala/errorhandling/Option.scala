package errorhandling

import scala.{Option => _}

sealed trait Option[+A] {

  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(v) => Some(f(v))
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(x) => x
  }

  def flatMap[B](f: A => Option[B]): Option[B] = map(f).getOrElse(None)

  def orElse[B >: A](ob: => Option[B]): Option[B] = this.map(Some(_)).getOrElse(ob)

  def filter(f: A => Boolean): Option[A] = flatMap(x => if(f(x)) Some(x) else None)

}

case class Some[A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {

  def mean(xs: Seq[Double]): Option[Double] = {
    if(xs.isEmpty) None
    else Some(xs.sum / xs.length)
  }
  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x-m, 2))))

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    a.flatMap(aa => b.map(bb => f(aa, bb)))
  }

  def map2for[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    for{
      aa <- a
      bb <- b
    } yield f(aa, bb)
  }

  def map3[A, B, C, D](a: Option[A], b: Option[B], c: Option[C])(f: (A, B, C) => D): Option[D] = {
    a.flatMap(aa => b.flatMap(bb => c.map(cc => f(aa, bb, cc))))
  }

  def map3for[A, B, C, D](a: Option[A], b: Option[B], c: Option[C])(f: (A, B, C) => D): Option[D] = {
    for {
      aa <- a
      bb <- b
      cc <- c
    } yield f(aa, bb, cc)
  }

  def insuaranceRateQuote(age: Int, numberOfSpeedTickets: Int): Double = age * numberOfSpeedTickets

  def parseInsuranceRateQuote(age: String, numberOfSpeedTickets: String): Option[Double] = {
    val optAge: Option[Int] = Try{age.toInt}
    val optTickets: Option[Int] = Try{numberOfSpeedTickets.toInt}
    map2(optAge, optTickets)(insuaranceRateQuote)
//    map2for(optAge, optTickets)(insuaranceRateQuote)
  }

  def calcXXX(age: Int, id: Int, ticket: Int): Double = age + id + ticket

  def xxx(age: String, id: String, ticketNumber: String): Option[Double] = {
    val optAge: Option[Int] = Try{age.toInt}
    val optId: Option[Int] = Try{id.toInt}
    val optTkt: Option[Int] = Try{ticketNumber.toInt}
    map3(optAge, optId, optTkt)(calcXXX)
  }

  def Try[A](a: => A): Option[A] = {
    try Some(a)
    catch {case e: Exception => None}
  }

  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    a.foldRight[Option[List[A]]](Some(Nil))((x, y) => map2(x, y)(_ :: _))
  }

  def parseInts(a: List[String]): Option[List[Int]] = {
    sequence(a.map(i => Try{i.toInt}))
  }

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    a.foldRight[Option[List[B]]](Some(Nil))((x, y) => map2(f(x), y)(_ :: _))
  }

}