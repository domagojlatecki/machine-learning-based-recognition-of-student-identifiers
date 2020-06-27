package at.doml.thesis.nn

import at.doml.thesis.util.collection.sized.Vec
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should
import scala.collection.immutable.ArraySeq

final class NeuronSpec extends AnyFreeSpec with should.Matchers {

  "A Neuron" - {

    "should correctly calculate output for some input Vec" in {
      val neuron = Neuron(Vec.unsafeWrap[Double, 3](ArraySeq(1.0, 2.0, 3.0)), 10.0)
      val input = Vec.unsafeWrap[Double, 3](ArraySeq(-1.0, 2.0, -10.0))
      val expectedSum = 1.0 * input.underlying(0) + 2.0 * input.underlying(1) + 3.0 * input.underlying(2) + 10.0
      val expectedResult = 1.0 / (1.0 + math.exp(-expectedSum))

      neuron.out(input) shouldBe (expectedResult +- 10e-7)
    }

    "should be correctly generated using specified random weight range" in {
      (1 to 100).foreach { _ =>
        val neuron = Neuron.random(5, (-0.5, 0.5))

        neuron.w.length shouldBe 5
        neuron.w0 should (be >= -0.5 and be <= 0.5)
        all (neuron.w.underlying) should (be >= -0.5 and be <= 0.5)
      }
    }
  }
}
