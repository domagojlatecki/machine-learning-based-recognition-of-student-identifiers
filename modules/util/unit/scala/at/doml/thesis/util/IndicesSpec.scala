package at.doml.thesis.util

import at.doml.thesis.util.collection.sized.Indices
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should
import scala.collection.immutable.ArraySeq
import scala.collection.mutable.ListBuffer

class IndicesSpec extends AnyFreeSpec with should.Matchers {

  "Indices" - {

    "should correctly wrap some range" in {
      Indices.until(0).underlying shouldBe (0 until 0)
      Indices.until(1).underlying shouldBe (0 until 1)
      Indices.until(2).underlying shouldBe (0 until 2)
      Indices.until(3).underlying shouldBe (0 until 3)
      Indices.until(4).underlying shouldBe (0 until 4)
      Indices.until(5).underlying shouldBe (0 until 5)
    }

    "should correctly map to a Vec" in {
      Indices.until(0).map(_.v).underlying shouldBe ArraySeq.empty
      Indices.until(1).map(_.v).underlying shouldBe ArraySeq(0)
      Indices.until(2).map(_.v).underlying shouldBe ArraySeq(0, 1)
      Indices.until(3).map(_.v).underlying shouldBe ArraySeq(0, 1, 2)
      Indices.until(4).map(_.v).underlying shouldBe ArraySeq(0, 1, 2, 3)
      Indices.until(5).map(_.v).underlying shouldBe ArraySeq(0, 1, 2, 3, 4)
    }

    "should correctly iterate using foreach" in {
      val acc = ListBuffer.empty[Int]

      Indices.until(0).foreach(acc += _.v)
      acc.toList shouldBe Nil

      acc.clear()
      Indices.until(1).foreach(acc += _.v)
      acc.toList shouldBe List(0)

      acc.clear()
      Indices.until(2).foreach(acc += _.v)
      acc.toList shouldBe List(0, 1)

      acc.clear()
      Indices.until(3).foreach(acc += _.v)
      acc.toList shouldBe List(0, 1, 2)

      acc.clear()
      Indices.until(4).foreach(acc += _.v)
      acc.toList shouldBe List(0, 1, 2, 3)

      acc.clear()
      Indices.until(5).foreach(acc += _.v)
      acc.toList shouldBe List(0, 1, 2, 3, 4)
    }
  }
}
