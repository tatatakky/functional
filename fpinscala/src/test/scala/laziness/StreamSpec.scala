package laziness

import org.scalatest.FunSpec

class StreamSpec extends FunSpec {

  val stream = Stream(1,2,3,4)
  val stream2 = Stream(5,6,7)
  val same = Stream(1,1,2,1)

  describe("Stream"){
    it("headOption : Stream(1,2,3,4).headOption === Some(1)"){
      assert(stream.headOption === Some(1))
    }
    val none = Empty
    it("headOption : Empty.headOption === None"){
      assert(none.headOption === None)
    }

    it("toList     : Stream(1,2,3,4).toList === List(1,2,3,4)"){
      assert(stream.toList === List(1,2,3,4))
    }

    it("take       : Stream(1,2,3,4).take(2).toList === List(1,2)"){
      assert(stream.take(2).toList === List(1,2))
    }

    it("drop       : Stream(1,2,3,4).drop(2).toList === List(3,4)"){
      assert(stream.drop(2).toList === List(3,4))
    }

    it("takeWhile  : Stream(1,2,3,4).takeWhile(_ < 3).toList === List(1,2)"){
      assert(stream.takeWhile(_ < 3).toList === List(1,2))
    }

    it("exists     : Strean(1,2,3,4).exists(_ == 3) === True") {
      assert(stream.exists(_ == 3))
    }

    it("forAll     : Stream(1,1,2,1).forAll(_ == 1) === False"){
      assert(!same.forAll(_ == 1))
    }

    it("map        : Stream(1,2,3,4).map(i => i*2).toList === List(2,4,6,8)"){
      assert(stream.map(i => i*2).toList === List(2,4,6,8))
    }

    it("filter     : Stream(1,2,3,4).filter(i => i%2 == 0).toList === List(2,4)"){
      assert(stream.filter(i => i%2==0).toList === List(2,4))
    }

    it("append     : Stream(1,2,3,4).append(Stream(5,6,7)).toList === List(1,2,3,4,5,6,7))"){
      assert(stream.append(stream2).toList === List(1,2,3,4,5,6,7))
    }

    val ss = Stream(stream, stream2)
    it("flatMap    : Stream(Stream(1,2,3,4), Stream(5,6,7)).flatMap(i => Stream(i)).toList === List(1,2,3,4,5,6,7)"){
      assert(ss.flatMap(i => i).toList === List(1,2,3,4,5,6,7))
    }

    it("find       : Stream(1,2,3,4).find(_%2==0) == Some(2)"){
      assert(stream.find(_%2==0) === Some(2))
    }

    it("map+filter : Stream(1,2,3,4).map(_ + 10).filter(_%2==0).toList === List(12,14)"){
      assert(stream.map(_ + 10).filter(_%2==0).toList === List(12,14))
    }
  }


//  val ones: Stream[Int] = Stream.cons(1, ones)
  import Stream._
  describe("Infinite Stream and Corecursion"){
    it("ones                   : Stream.cons(1,ones).take(5).toList === List(1,1,1,1,1)"){
      assert(ones.take(5).toList === List(1,1,1,1,1))
    }

    it("map+exists             : ones.map(_ + 1).exists(_ % 2 == 0) === True"){
      assert(ones.map(_ + 1).exists(_%2 == 0))
    }

    it("take + takeWhile       : ones.takeWhile(_ == 1) === List(1,1,1,1,1)"){
      assert(ones.take(5).takeWhile(_ == 1).toList === List(1,1,1,1,1))
    }

    it("forAll                 : ones.forAll(_ != 1) === False"){
      assert(!ones.forAll(_ != 1))
    }

    it("constant               : constant(a).take(5).toList === List(a,a,a,a,a)"){
      assert(constant("a").take(5).toList === List("a", "a", "a", "a", "a"))
    }

    it("from                   : from(2).take(5).toList === List(2,3,4,5,6)"){
      assert(from(2).take(5).toList === List(2,3,4,5,6))
    }

    it("fibs                   : fibs.take(7).toList === List(0,1,1,2,3,5,8))"){
      assert(fibs.take(7).toList === List(0,1,1,2,3,5,8))
    }

    it("unfold                 : unfold(1)(x => Some(x, x+2)).take(5).toList === List(1,2,3,4,5)"){
      assert(unfold(1)(e => Some(e, e+2)).take(5).toList === List(1,3,5,7,9))
    }

    it("mapUnfoldX             : mapUnfoldX(stream)(_ * 2).toList === List(2,4,6,8)"){
      assert(mapUnfoldX(stream)(_ * 2).toList === List(2,4,6,8))
    }

    it("mapUnfold              : stream.mapUnfold(_ * 2).toList === List(2,4,6,8)"){
      assert(stream.mapUnfold(_ * 2).toList === List(2,4,6,8))
    }

    it("fibs + take            : fibs.take(7).toList === List(0,1,1,2,3,5,8)"){
      assert(fibs.take(7).toList === List(0,1,1,2,3,5,8))
    }

    it("fibs + take + takeWhile: fibs.take(8).takeWhile(_ < 10).toList === List(0,1,1,2,3,5,8)"){
      assert(fibs.take(8).takeWhile(_ < 10).toList === List(0,1,1,2,3,5,8))
    }

    it("zipWith                : from(1).take(7).zipWith(fibs.take(7))(_ + _).toList === List(1,3,4,6,8,11,15)"){
      assert(from(1).take(7).zipWith(fibs.take(7))(_ + _).toList === List(1,3,4,6,8,11,15))
    }

    it("zipAll                 : from(1).take(7).zipAll(fibs.take(6)).toList === List((Some(1),Some(0)), (Some(2),Some(1)), (Some(3),Some(1)), (Some(4),Some(2)), (Some(5),Some(3)), (Some(6),None), (Some(7),None))"){
      assert(from(1).take(7).zipAll(fibs.take(5)).toList === List((Some(1),Some(0)), (Some(2),Some(1)), (Some(3),Some(1)), (Some(4),Some(2)), (Some(5),Some(3)), (Some(6),None), (Some(7),None)))
    }

    it("startsWith             : from(1).take(4).startsWith(from(1).take(2)) === True"){
      assert(from(1).take(4).startsWith(from(1).take(2)))
    }

    it("tails                  : Stream(1,2,3,4).tails.toList.map(_.toList) === List(List(1, 2, 3, 4), List(2, 3, 4), List(3, 4), List(4))"){
      assert(stream.tails.toList.map(_.toList) === List(List(1, 2, 3, 4), List(2, 3, 4), List(3, 4), List(4)))
    }

    val stream2 = Stream(2,3)
    it("hasSubSequence         : Stream(1,2,3,4).hasSubSequence(Stream(2,3)) === True"){
      assert(stream.hasSubSequence(stream2))
    }

    it("scanRight              : Stream(1,2,3,4).scanRight(0)(_ + _).toList === List(10, 9, 7, 4, 0)"){
      assert(stream.scanRight(0)(_ + _).toList === List(10, 9, 7, 4, 0))
    }

  }

}