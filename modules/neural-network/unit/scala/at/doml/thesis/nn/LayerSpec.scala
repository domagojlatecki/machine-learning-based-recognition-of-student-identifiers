package at.doml.thesis.nn

import at.doml.thesis.util.collection.sized.Vec
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should
import scala.collection.immutable.ArraySeq
import scala.reflect.ClassTag

final class LayerSpec extends AnyFreeSpec with should.Matchers {

  private implicit final class DoubleOps(v: Double) {
    def logistic: Double = 1.0 / (1.0 + math.exp(-v))
  }

  private def vec[A : ClassTag, S <: Int](values: A*): Vec[A, S] = {
    Vec.unsafeWrap[A, S](values.to(ArraySeq))
  }

  "A Layer" - {

    "should correctly calculate output for some input Vec" in {
      val layer = Layer(
        vec[Neuron[2], 3](
          Neuron(vec[Double, 2](1.0, 2.0), 10.0),
          Neuron(vec[Double, 2](3.0, 4.0), 100.0),
          Neuron(vec[Double, 2](5.0, 6.0), 1000.0)
        )
      )
      val input        = vec[Double, 2](2.5, -1.0)
      val expectedSum1 = 1.0 * input.underlying(0) + 2.0 * input.underlying(1) + 10.0
      val expectedSum2 = 3.0 * input.underlying(0) + 4.0 * input.underlying(1) + 100.0
      val expectedSum3 = 5.0 * input.underlying(0) + 6.0 * input.underlying(1) + 1000.0
      val result       = layer.out(input)

      result.length shouldBe 3
      result.underlying(0) shouldBe (expectedSum1.logistic +- 10e-7)
      result.underlying(1) shouldBe (expectedSum2.logistic +- 10e-7)
      result.underlying(2) shouldBe (expectedSum3.logistic +- 10e-7)
    }

    "should be correctly generated using specified random weight range" in {
      (1 to 100).foreach { _ =>
        val layer = Layer.random(5, 5, (-0.5, 0.5))

        layer.neurons.length shouldBe 5

        val n1 = layer.neurons.underlying(0)
        n1.w.length shouldBe 5
        n1.w0 should (be >= -0.5 and be <= 0.5)
        all(n1.w.underlying) should (be >= -0.5 and be <= 0.5)

        val n2 = layer.neurons.underlying(1)
        n2.w.length shouldBe 5
        n2.w0 should (be >= -0.5 and be <= 0.5)
        all(n2.w.underlying) should (be >= -0.5 and be <= 0.5)

        val n3 = layer.neurons.underlying(2)
        n3.w.length shouldBe 5
        n3.w0 should (be >= -0.5 and be <= 0.5)
        all(n3.w.underlying) should (be >= -0.5 and be <= 0.5)

        val n4 = layer.neurons.underlying(3)
        n4.w.length shouldBe 5
        n4.w0 should (be >= -0.5 and be <= 0.5)
        all(n4.w.underlying) should (be >= -0.5 and be <= 0.5)

        val n5 = layer.neurons.underlying(4)
        n5.w.length shouldBe 5
        n5.w0 should (be >= -0.5 and be <= 0.5)
        all(n5.w.underlying) should (be >= -0.5 and be <= 0.5)
      }
    }
  }
}
