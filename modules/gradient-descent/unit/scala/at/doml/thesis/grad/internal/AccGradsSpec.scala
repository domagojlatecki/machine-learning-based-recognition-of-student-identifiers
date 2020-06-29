package at.doml.thesis.grad.internal

import at.doml.thesis.grad.LayerGradsUtil
import at.doml.thesis.grad.LayerGradsUtil.LayerOps
import at.doml.thesis.nn.NeuralNetwork
import at.doml.thesis.util.collection.sized.Vec
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should
import scala.annotation.tailrec
import scala.collection.immutable.ArraySeq

final class AccGradsSpec extends AnyFreeSpec with should.Matchers {

  @tailrec
  private def compare[I <: Int, O <: Int](nn: NeuralNetwork[I, O], acc: AccGrads[I, O], v: Vec[Double, I]): Unit = {
    (nn, acc) match {

      case (NeuralNetwork.ForwardPass(nnFirst, nnRest), AccGrads.ForwardPass(accFirst, accRest)) =>
        val nnOut  = nnFirst.out(v)
        val accOut = accFirst.out(v)

        accOut.length shouldBe nnOut.length
        accOut.underlying shouldBe nnOut.underlying
        accFirst.layer.weights shouldBe nnFirst.weights
        compare(nnRest, accRest, nnOut)

      case (NeuralNetwork.LastLayer(nnLayer), AccGrads.LastLayer(accLayer)) =>
        val nnOut  = nnLayer.out(v)
        val accOut = accLayer.out(v)

        accOut.length shouldBe nnOut.length
        accOut.underlying shouldBe nnOut.underlying
        accLayer.layer.weights shouldBe nnLayer.weights

      case (nnType, accType) =>
        fail(s"Mismatching NeuralNetwork and AccGrads: ${nnType.productPrefix}, ${accType.productPrefix}")
    }
  }

  "AccGrads" - {

    "should correctly wrap some neural network" in {
      val neuralNetwork: NeuralNetwork[3, 5] = NeuralNetwork.random(3, List(2, 1), 5, (-0.5, 0.5))
      val accGrads: AccGrads[3, 5]           = AccGrads.wrap(neuralNetwork)

      val input  = Vec.unsafeWrap[Double, 3](ArraySeq(1.0, 2.0, 3.0))
      val nnOut  = neuralNetwork.out(input)
      val accOut = accGrads.out(input)

      accOut.length shouldBe nnOut.length
      accOut.underlying shouldBe nnOut.underlying

      compare(neuralNetwork, accGrads, input)
    }

    "should correctly unwrap into some neural network" in {
      val lastLayer = AccGrads.LastLayer(LayerGradsUtil.randomLayerGrands(in = 1, out = 5, gradValue = 0.0))

      val middleLayer =
        AccGrads.ForwardPass(LayerGradsUtil.randomLayerGrands(in = 2, out = 1, gradValue = 0.0), lastLayer)

      val accGrads: AccGrads[3, 5] =
        AccGrads.ForwardPass(LayerGradsUtil.randomLayerGrands(in = 3, out = 2, gradValue = 0.0), middleLayer)

      val neuralNetwork: NeuralNetwork[3, 5] = AccGrads.unwrap(accGrads)
      val input                              = Vec.unsafeWrap[Double, 3](ArraySeq(1.0, 2.0, 3.0))
      val nnOut                              = neuralNetwork.out(input)
      val accOut                             = accGrads.out(input)

      accOut.length shouldBe nnOut.length
      accOut.underlying shouldBe nnOut.underlying

      compare(neuralNetwork, accGrads, input)
    }

    "should correctly wrap and then unwrap some neural network" in {
      val neuralNetwork: NeuralNetwork[3, 5] = NeuralNetwork.random(3, List(2, 1), 5, (-0.5, 0.5))
      val accGrads: AccGrads[3, 5]           = AccGrads.wrap(neuralNetwork)
      val unwrapped: NeuralNetwork[3, 5]     = AccGrads.unwrap(accGrads)

      val input = Vec.unsafeWrap[Double, 3](ArraySeq(1.0, 2.0, 3.0))
      val nnOut = neuralNetwork.out(input)
      val uwOut = unwrapped.out(input)

      nnOut.length shouldBe uwOut.length
      nnOut.underlying shouldBe uwOut.underlying

      @tailrec
      def compare[I <: Int, O <: Int](nn: NeuralNetwork[I, O], uw: NeuralNetwork[I, O], v: Vec[Double, I]): Unit = {
        (nn, uw) match {

          case (NeuralNetwork.ForwardPass(nnFirst, nnRest), NeuralNetwork.ForwardPass(uwFirst, uwRest)) =>
            val nnOut = nnFirst.out(v)
            val uwOut = uwFirst.out(v)

            uwOut.length shouldBe nnOut.length
            uwOut.underlying shouldBe nnOut.underlying
            uwFirst.weights shouldBe nnFirst.weights
            compare(nnRest, uwRest, nnOut)

          case (NeuralNetwork.LastLayer(nnLayer), NeuralNetwork.LastLayer(uwLayer)) =>
            val nnOut = nnLayer.out(v)
            val uwOut = uwLayer.out(v)

            uwOut.length shouldBe nnOut.length
            uwOut.underlying shouldBe nnOut.underlying
            uwLayer.weights shouldBe nnLayer.weights

          case (nnType, uwType) =>
            fail(s"Mismatching original and unwrapped NeuralNetworks: ${nnType.productPrefix}, ${uwType.productPrefix}")
        }
      }

      compare(neuralNetwork, unwrapped, input)
    }
  }
}
