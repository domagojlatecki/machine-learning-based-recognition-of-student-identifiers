package at.doml.thesis.grad.calc

import at.doml.thesis.grad.LayerGradsUtil
import at.doml.thesis.grad.LayerGradsUtil.{ LayerOps, VecOfNeuronGradsOps }
import at.doml.thesis.grad.internal.AccGrads.{ ForwardPass, LastLayer }
import at.doml.thesis.grad.internal.NeuralNetworkData.{ BackwardPassData, FirstLayerData }
import at.doml.thesis.grad.internal.{ AccGrads, NeuralNetworkData }
import at.doml.thesis.nn.Layer
import at.doml.thesis.util.collection.sized.Vec
import at.doml.thesis.util.par.Parallel
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should
import scala.collection.immutable.ArraySeq

final class LayerOutputsSpec extends AnyFreeSpec with should.Matchers {

  private implicit object TestParallel extends Parallel {
    override def itemsPerThread: Int = Int.MaxValue
    override def execute[A](tasks: Iterator[() => A]): List[A] = tasks.map(_.apply()).toList
    override def shutdown(): Unit = ()
  }

  "LayerOutputs" - {

    "should correctly calculate neural network layer outputs" in {
      val lastLayer = AccGrads.LastLayer(LayerGradsUtil.randomLayerGrands(in = 1, out = 5, gradValue = math.random()))

      val middleLayer =
        AccGrads.ForwardPass(LayerGradsUtil.randomLayerGrands(in = 2, out = 1, gradValue = math.random()), lastLayer)

      val accGrads: AccGrads[3, 5] =
        AccGrads.ForwardPass(LayerGradsUtil.randomLayerGrands(in = 3, out = 2, gradValue = math.random()), middleLayer)

      val inputs: Vec[Vec[Double, 3], 2] =
        Vec.unsafeWrap(ArraySeq(Vec.unsafeWrap(ArraySeq(1.0, 2.0, 3.0)), Vec.unsafeWrap(ArraySeq(4.0, 5.0, 6.0))))

      val data: NeuralNetworkData[3, 5, 2] = LayerOutputs(inputs, accGrads)

      accGrads shouldBe a[ForwardPass[_, _, _]]
      data shouldBe a[BackwardPassData[_, _, _, _]]

      val BackwardPassData(lastLayerData, middleData) = data.asInstanceOf[BackwardPassData[3, 1, 5, 2]]
      val ForwardPass(firstLayerAcc, middleAcc)       = accGrads.asInstanceOf[ForwardPass[3, 2, 5]]

      middleData shouldBe a[BackwardPassData[_, _, _, _]]
      middleAcc shouldBe a[ForwardPass[_, _, _]]

      val BackwardPassData(middleLayerData, firstData) = middleData.asInstanceOf[BackwardPassData[3, 2, 1, 2]]
      val ForwardPass(middleLayerAcc, lastAcc)         = middleAcc.asInstanceOf[ForwardPass[2, 1, 5]]

      firstData shouldBe a[FirstLayerData[_, _, _]]
      lastAcc shouldBe a[LastLayer[_, _]]

      val FirstLayerData(firstLayerData) = firstData.asInstanceOf[FirstLayerData[3, 2, 2]]
      val LastLayer(lastLayerAcc)        = lastAcc.asInstanceOf[LastLayer[1, 5]]

      val firstInData      = firstLayerData.sampleInputs
      val firstOutData     = firstLayerData.sampleOutputs
      val firstNeuronsData = Layer(firstLayerData.neurons)
      val firstGradsData   = firstLayerData.accGrads

      firstInData.underlying shouldBe inputs.underlying
      firstOutData.length shouldBe 2
      firstOutData.underlying(0) shouldBe firstLayerAcc.layer.out(inputs.underlying(0))
      firstOutData.underlying(1) shouldBe firstLayerAcc.layer.out(inputs.underlying(1))
      firstNeuronsData.weights shouldBe firstLayerAcc.layer.weights
      firstGradsData.weights shouldBe firstLayerAcc.accGrads.weights

      val middleInData      = middleLayerData.sampleInputs
      val middleOutData     = middleLayerData.sampleOutputs
      val middleNeuronsData = Layer(middleLayerData.neurons)
      val middleGradsData   = middleLayerData.accGrads

      middleInData.underlying shouldBe firstOutData.underlying
      middleOutData.length shouldBe 2
      middleOutData.underlying(0) shouldBe middleLayerAcc.layer.out(firstOutData.underlying(0))
      middleOutData.underlying(1) shouldBe middleLayerAcc.layer.out(firstOutData.underlying(1))
      middleNeuronsData.weights shouldBe middleLayerAcc.layer.weights
      middleGradsData.weights shouldBe middleLayerAcc.accGrads.weights

      val lastInData      = lastLayerData.sampleInputs
      val lastOutData     = lastLayerData.sampleOutputs
      val lastNeuronsData = Layer(lastLayerData.neurons)
      val lastGradsData   = lastLayerData.accGrads

      lastInData.underlying shouldBe middleOutData.underlying
      lastOutData.length shouldBe 2
      lastOutData.underlying(0) shouldBe lastLayerAcc.layer.out(middleOutData.underlying(0))
      lastOutData.underlying(1) shouldBe lastLayerAcc.layer.out(middleOutData.underlying(1))
      lastNeuronsData.weights shouldBe lastLayerAcc.layer.weights
      lastGradsData.weights shouldBe lastLayerAcc.accGrads.weights
    }
  }
}
