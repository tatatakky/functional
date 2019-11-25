package monoids

object MonoidMain {

  def main(args: Array[String]): Unit = {

    import Monoid._

    val words = List("ABC", "DE", "FGH")
    val s = words.foldRight(stringMonoid.zero)(stringMonoid.op)
    val t = words.foldLeft(stringMonoid.zero)(stringMonoid.op)
    println(s)
    println(t)

    val list: List[String] = List("1", "2", "3")
    val res = foldMap(list, intAddition)(_.toInt)
    println(res)

    val seq = IndexedSeq("1", "2", "3", "4", "5", "6", "7", "8")
    val res2 = foldMapV(seq, intAddition)(_.toInt)
    println(res2)

    val str = "This is a pen which I bought yesterday"
    println(wordCount(str))

    val tup = Set("aa", "bb").foreach(x => println(x.getClass))

    val intList = List(1, 2, 3, 4)
    val pm = productMonoid(intAddition, intAddition)
    val res3 = ListFoldable.foldMap(intList)(a => (a, 1))(pm)
    println(res3)

  }
}