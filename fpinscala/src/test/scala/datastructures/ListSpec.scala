package datastructures

import org.scalatest._

class ListSpec extends WordSpec {

  val l: List[Int] = List(1, 2, 3, 4)
//
//  "tail in List, " should {
//    "when the arg is given List(1,2,3,4), result is List(2,3,4)" in {
//      import chapter3.List.tail
//      val x = tail(l)
//      assert(x == List(2, 3, 4))
//    }
//  }
//
//  "setHead in List, " should {
//    "then the arg is given (List(1, 2, 3, 4), 5), result is List(5,2,3,4)" in {
//      import chapter3.List.setHead
//      val x = setHead(l, 5)
//      assert(x == List(5, 2, 3, 4))
//    }
//  }
//
//  "drop in List, " should {
//    "when the arg is given (List(1,2,3,4), 2), result is List(3,4)" in {
//      import chapter3.List.drop
//      val x = drop(l, 2)
//      assert(x == List(3, 4))
//    }
//  }
//
//  "dropWhile in List, " should {
//    "a" in {
//      import chapter3.List.dropWhile
//      val x = dropWhile(l)(x => x<3)
//      assert(x == List(3, 4))
//    }
//  }
//
//  "init in List, " should {
//    "when the arg is given List(1,2,3,4), result is List(1,2,3)" in {
//      import chapter3.List.init
//      val x = init(l)
//      assert(x == List(1,2,3))
//    }
//  }
//
//  "foldRight in List, " should {
//    "is correct" in {
//      import chapter3.List.{foldRight, sum, product}
//      val x = sum(List(1,2,3,4))
//      val y = product(List(1,2,3,4))
//      assert(x == 10)
//      assert(y == 24)
//    }
//  }
//
//  "aaaa" should {
//    "aaaaaaa" in {
//      import chapter3.List.a
//      println(a(Nil))
//    }
//  }

}