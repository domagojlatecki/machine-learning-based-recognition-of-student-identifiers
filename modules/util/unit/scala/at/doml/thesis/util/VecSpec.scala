package at.doml.thesis.util

import at.doml.thesis.util.collection.sized.Vec
import at.doml.thesis.util.par.Parallel
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should
import scala.collection.immutable.ArraySeq
import scala.collection.mutable.ListBuffer

final class VecSpec extends AnyFreeSpec with should.Matchers {

  private implicit object TestParallel extends Parallel {
    override def itemsPerThread: Int                           = Int.MaxValue
    override def execute[A](tasks: Iterator[() => A]): List[A] = tasks.map(_.apply()).toList
    override def shutdown(): Unit                              = ()
  }

  "A Vec" - {

    "(when empty)" - {

      "should have length 0" - {

        "[using Vec.empty]" in {
          Vec.empty[Int].length shouldBe 0
        }

        "[using Vec.unsafeWrap]" in {
          Vec.unsafeWrap[Int, 0](ArraySeq.empty[Int]).length shouldBe 0
        }

        "[using Vec.fill]" in {
          Vec.fill(0)(0).length shouldBe 0
        }
      }

      "should return correct indices" - {

        "[using Vec.empty]" in {
          Vec.empty[Int].indices.underlying shouldBe (0 until 0)
        }

        "[using Vec.unsafeWrap]" in {
          Vec.unsafeWrap[Int, 0](ArraySeq.empty[Int]).indices.underlying shouldBe (0 until 0)
        }

        "[using Vec.fill]" in {
          Vec.fill(0)(0).indices.underlying shouldBe (0 until 0)
        }
      }
    }

    "(when non-empty)" - {

      "should return correct length" - {

        "[using Vec.unsafeWrap]" in {
          Vec.unsafeWrap[Int, 1](ArraySeq.fill(1)(0)).length shouldBe 1
          Vec.unsafeWrap[Int, 2](ArraySeq.fill(2)(0)).length shouldBe 2
          Vec.unsafeWrap[Int, 3](ArraySeq.fill(3)(0)).length shouldBe 3
          Vec.unsafeWrap[Int, 4](ArraySeq.fill(4)(0)).length shouldBe 4
          Vec.unsafeWrap[Int, 5](ArraySeq.fill(5)(0)).length shouldBe 5
        }

        "[using Vec.fill]" in {
          Vec.fill(1)(0).length shouldBe 1
          Vec.fill(2)(0).length shouldBe 2
          Vec.fill(3)(0).length shouldBe 3
          Vec.fill(4)(0).length shouldBe 4
          Vec.fill(5)(0).length shouldBe 5
        }
      }

      "should return correct indices" - {

        "[using Vec.unsafeWrap]" in {
          Vec.unsafeWrap[Int, 1](ArraySeq.fill(1)(0)).indices.underlying shouldBe (0 until 1)
          Vec.unsafeWrap[Int, 2](ArraySeq.fill(2)(0)).indices.underlying shouldBe (0 until 2)
          Vec.unsafeWrap[Int, 3](ArraySeq.fill(3)(0)).indices.underlying shouldBe (0 until 3)
          Vec.unsafeWrap[Int, 4](ArraySeq.fill(4)(0)).indices.underlying shouldBe (0 until 4)
          Vec.unsafeWrap[Int, 5](ArraySeq.fill(5)(0)).indices.underlying shouldBe (0 until 5)
        }

        "[using Vec.fill]" in {
          Vec.fill(1)(0).indices.underlying shouldBe (0 until 1)
          Vec.fill(2)(0).indices.underlying shouldBe (0 until 2)
          Vec.fill(3)(0).indices.underlying shouldBe (0 until 3)
          Vec.fill(4)(0).indices.underlying shouldBe (0 until 4)
          Vec.fill(5)(0).indices.underlying shouldBe (0 until 5)
        }
      }

      "should return correct elements when iterating using indices" - {

        "[using Vec.unsafeWrap]" in {
          val acc = ListBuffer.empty[Int]

          val vec1 = Vec.unsafeWrap[Int, 1](ArraySeq(0))
          for (i <- vec1.indices) {
            acc += vec1(i)
          }
          acc.toList shouldBe List(0)

          acc.clear()
          val vec2 = Vec.unsafeWrap[Int, 2](ArraySeq(0, 1))
          for (i <- vec2.indices) {
            acc += vec2(i)
          }
          acc.toList shouldBe List(0, 1)

          acc.clear()
          val vec3 = Vec.unsafeWrap[Int, 3](ArraySeq(0, 1, 2))
          for (i <- vec3.indices) {
            acc += vec3(i)
          }
          acc.toList shouldBe List(0, 1, 2)

          acc.clear()
          val vec4 = Vec.unsafeWrap[Int, 4](ArraySeq(0, 1, 2, 3))
          for (i <- vec4.indices) {
            acc += vec4(i)
          }
          acc.toList shouldBe List(0, 1, 2, 3)

          acc.clear()
          val vec5 = Vec.unsafeWrap[Int, 5](ArraySeq(0, 1, 2, 3, 4))
          for (i <- vec5.indices) {
            acc += vec5(i)
          }
          acc.toList shouldBe List(0, 1, 2, 3, 4)
        }

        "[using Vec.fill]" in {
          val vec1 = Vec.fill(1)(-1)
          for (i <- vec1.indices) {
            vec1(i) shouldBe -1
          }

          val vec2 = Vec.fill(1)(-1)
          for (i <- vec2.indices) {
            vec2(i) shouldBe -1
          }

          val vec3 = Vec.fill(1)(-1)
          for (i <- vec3.indices) {
            vec3(i) shouldBe -1
          }

          val vec4 = Vec.fill(1)(-1)
          for (i <- vec4.indices) {
            vec4(i) shouldBe -1
          }

          val vec5 = Vec.fill(1)(-1)
          for (i <- vec5.indices) {
            vec5(i) shouldBe -1
          }
        }
      }

      "should correctly map values" - {

        "[using map]" in {
          Vec.unsafeWrap(ArraySeq(0, 1, 2, 3)).map(_.toString).underlying shouldBe ArraySeq("0", "1", "2", "3")
        }

        "[using parMap]" in {
          Vec.unsafeWrap(ArraySeq(0, 1, 2, 3)).parMap(_.toString).underlying shouldBe ArraySeq("0", "1", "2", "3")
        }
      }

      "should correctly map with another Vec" - {

        "[using mapWith]" in {
          val vec1 = Vec.unsafeWrap(ArraySeq(0, 1, 2, 3))
          val vec2 = Vec.unsafeWrap(ArraySeq(4, 5, 6, 7))

          vec1.mapWith(vec2)(_.toString + _.toString).underlying shouldBe ArraySeq("04", "15", "26", "37")
          vec2.mapWith(vec1)(_.toString + _.toString).underlying shouldBe ArraySeq("40", "51", "62", "73")
        }

        "[using parMapWith]" in {
          val vec1 = Vec.unsafeWrap(ArraySeq(0, 1, 2, 3))
          val vec2 = Vec.unsafeWrap(ArraySeq(4, 5, 6, 7))

          vec1.parMapWith(vec2)(_.toString + _.toString).underlying shouldBe ArraySeq("04", "15", "26", "37")
          vec2.parMapWith(vec1)(_.toString + _.toString).underlying shouldBe ArraySeq("40", "51", "62", "73")
        }
      }

      "should correctly map with another two Vecs" in {
        val vec1 = Vec.unsafeWrap(ArraySeq(0, 1, 2))
        val vec2 = Vec.unsafeWrap(ArraySeq(3, 4, 5))
        val vec3 = Vec.unsafeWrap(ArraySeq(6, 7, 8))

        vec1.mapWith(vec2, vec3)(_.toString + _.toString + _.toString).underlying shouldBe ArraySeq("036", "147", "258")
        vec1.mapWith(vec3, vec2)(_.toString + _.toString + _.toString).underlying shouldBe ArraySeq("063", "174", "285")
        vec2.mapWith(vec1, vec3)(_.toString + _.toString + _.toString).underlying shouldBe ArraySeq("306", "417", "528")
        vec2.mapWith(vec3, vec1)(_.toString + _.toString + _.toString).underlying shouldBe ArraySeq("360", "471", "582")
        vec3.mapWith(vec1, vec2)(_.toString + _.toString + _.toString).underlying shouldBe ArraySeq("603", "714", "825")
        vec3.mapWith(vec2, vec1)(_.toString + _.toString + _.toString).underlying shouldBe ArraySeq("630", "741", "852")
      }

      "should correctly map with index" - {

        "[using mapWithIndex]" in {
          Vec
            .unsafeWrap[Int, 5](ArraySeq(-1, -2, -3, -4, -5))
            .mapWithIndex(_.toString + _.v.toString).underlying shouldBe ArraySeq("-10", "-21", "-32", "-43", "-54")
        }

        "[using parMapWithIndex]" in {
          Vec
            .unsafeWrap[Int, 5](ArraySeq(-1, -2, -3, -4, -5))
            .parMapWithIndex(_.toString + _.v.toString).underlying shouldBe ArraySeq("-10", "-21", "-32", "-43", "-54")
        }
      }

      "should correctly return minimum value" in {
        val vec = Vec.unsafeWrap(
          ArraySeq(
            (-1, "a"),
            (-2, "b"),
            (-3, "c"),
            (-4, "d"),
            (-5, "e"),
            (0, "f"),
            (1, "g"),
            (2, "h"),
            (3, "i"),
            (4, "j")
          )
        )

        vec.minBy(_._1) shouldBe ((-5, "e"))
        vec.minBy(_._2) shouldBe ((-1, "a"))
      }

      "should correctly return maximum value" in {
        val vec = Vec.unsafeWrap(
          ArraySeq(
            (-1, "j"),
            (-2, "i"),
            (-3, "h"),
            (-4, "g"),
            (-5, "f"),
            (0, "e"),
            (1, "d"),
            (2, "c"),
            (3, "b"),
            (4, "a")
          )
        )

        vec.maxBy(_._1) shouldBe ((4, "a"))
        vec.maxBy(_._2) shouldBe ((-1, "j"))
      }
    }
  }
}
