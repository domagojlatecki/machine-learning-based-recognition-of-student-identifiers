package at.doml.thesis.nn

import at.doml.thesis.nn.NeuralNetwork.{ForwardPass, LastLayer}
import at.doml.thesis.util.collection.sized.Vec
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should
import scala.collection.immutable.ArraySeq
import scala.reflect.ClassTag

final class NeuralNetworkSpec extends AnyFreeSpec with should.Matchers  {

  private final implicit class DoubleOps(v: Double) {
    def logistic: Double = 1.0 / (1.0 + math.exp(-v))
  }

  private def vec[A : ClassTag, S <: Int](values: A*): Vec[A, S] = {
    Vec.unsafeWrap[A, S](values.to(ArraySeq))
  }

  "A NeuralNetwork" - {

    "should correctly calculate output for some input Vec" in {
      val inLayer = Layer[2, 2](
        vec[Neuron[2], 2](
          Neuron[2](
            vec[Double, 2](1.0, 2.0),
            10.0
          ),
          Neuron[2](
            vec[Double, 2](3.0, 4.0),
            100.0
          )
        )
      )
      val middleLayer = Layer[2, 3](
        vec[Neuron[2], 3](
          Neuron[2](
            vec[Double, 2](-1.0, -2.0),
            -10.0
          ),
          Neuron[2](
            vec[Double, 2](-3.0, -4.0),
            -100.0
          ),
          Neuron[2](
            vec[Double, 2](-5.0, -6.0),
            -1000.0
          )
        )
      )
      val outLayer = Layer[3, 2](
        vec[Neuron[3], 2](
          Neuron[3](
            vec[Double, 3](10.0, 20.0, 30.0),
            1.0
          ),
          Neuron[3](
            vec[Double, 3](40.0, 50.0, 60.0),
            2.0
          )
        )
      )

      val input = vec[Double, 2](-1.5, 2.5)
      val inLayerSum1 = 1.0 * input.underlying(0) + 2.0 * input.underlying(1) + 10.0
      val inLayerSum2 = 3.0 * input.underlying(0) + 4.0 * input.underlying(1) + 100.0

      val middleLayerSum1 = -1.0 * inLayerSum1.logistic - 2.0 * inLayerSum2.logistic - 10.0
      val middleLayerSum2 = -3.0 * inLayerSum1.logistic - 4.0 * inLayerSum2.logistic - 100.0
      val middleLayerSum3 = -5.0 * inLayerSum1.logistic - 6.0 * inLayerSum2.logistic - 1000.0

      val outLayerSum1 =
        10.0 * middleLayerSum1.logistic + 20.0 * middleLayerSum2.logistic + 30.0 * middleLayerSum3.logistic + 1.0
      val outLayerSum2 =
        40.0 * middleLayerSum1.logistic + 25.0 * middleLayerSum2.logistic + 60.0 * middleLayerSum3.logistic + 2.0

      val neuralNetwork: NeuralNetwork[2, 2] = ForwardPass(inLayer, ForwardPass(middleLayer, LastLayer(outLayer)))
      val result = neuralNetwork.out(input)

      result.length shouldBe 2
      result.underlying(0) shouldBe (outLayerSum1.logistic +- 10e-7)
      result.underlying(1) shouldBe (outLayerSum2.logistic +- 10e-7)
    }

    "should be correctly generated using specified random weight range" in {
      (1 to 100).foreach { _ =>
        val neuralNetwork = NeuralNetwork.random(3, List(2, 4), 1, (-0.5, 0.5))

        neuralNetwork shouldBe a[ForwardPass[_, _, _]]
        val ForwardPass(layer1, remaining2Layers) = neuralNetwork.asInstanceOf[ForwardPass[3, 2, 1]]

        layer1.neurons.length shouldBe 2

        val l1n1 = layer1.neurons.underlying(0)
        l1n1.w.length shouldBe 3
        l1n1.w0 should (be >= -0.5 and be <= 0.5)
        all (l1n1.w.underlying) should (be >= -0.5 and be <= 0.5)

        val l1n2 = layer1.neurons.underlying(1)
        l1n2.w.length shouldBe 3
        l1n2.w0 should (be >= -0.5 and be <= 0.5)
        all (l1n2.w.underlying) should (be >= -0.5 and be <= 0.5)

        remaining2Layers shouldBe a[ForwardPass[_, _, _]]
        val ForwardPass(layer2, remainingLayer) = remaining2Layers.asInstanceOf[ForwardPass[2, 4, 1]]

        layer2.neurons.length shouldBe 4

        val l2n1 = layer2.neurons.underlying(0)
        l2n1.w.length shouldBe 2
        l2n1.w0 should (be >= -0.5 and be <= 0.5)
        all (l2n1.w.underlying) should (be >= -0.5 and be <= 0.5)

        val l2n2 = layer2.neurons.underlying(1)
        l2n2.w.length shouldBe 2
        l2n2.w0 should (be >= -0.5 and be <= 0.5)
        all (l2n2.w.underlying) should (be >= -0.5 and be <= 0.5)

        val l2n3 = layer2.neurons.underlying(2)
        l2n3.w.length shouldBe 2
        l2n3.w0 should (be >= -0.5 and be <= 0.5)
        all (l2n3.w.underlying) should (be >= -0.5 and be <= 0.5)

        val l2n4 = layer2.neurons.underlying(3)
        l2n4.w.length shouldBe 2
        l2n4.w0 should (be >= -0.5 and be <= 0.5)
        all (l2n4.w.underlying) should (be >= -0.5 and be <= 0.5)

        remainingLayer shouldBe a[LastLayer[_, _]]
        val LastLayer(layer3) = remainingLayer.asInstanceOf[LastLayer[4, 1]]

        layer3.neurons.length shouldBe 1

        val l3n1 = layer3.neurons.underlying(0)
        l3n1.w.length shouldBe 4
        l3n1.w0 should (be >= -0.5 and be <= 0.5)
        all (l3n1.w.underlying) should (be >= -0.5 and be <= 0.5)
      }
    }
  }
}
