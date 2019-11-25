package parallel

import org.scalatest.FunSpec

import java.util.concurrent.{ExecutorService, Executors, Future => JFuture}
import parallel.Par.wordCount

class WordCountSpec extends FunSpec {

  describe("wordCount"){
    it("Number of Thread is 5 "){
      val textFile: String = "/Users/kodai/scala/fp-in-scala/src/main/scala/parallel/data/sample.txt"
      val es: ExecutorService = Executors.newFixedThreadPool(5)
      val jf: JFuture[List[Int]] = Par.run(es)(wordCount(textFile))
      val result: List[Int] = jf.get

      es.shutdown()

      assert(result === List(4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22))
    }
  }

}