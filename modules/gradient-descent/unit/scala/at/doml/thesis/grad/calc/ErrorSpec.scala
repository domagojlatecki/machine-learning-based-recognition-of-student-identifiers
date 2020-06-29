package at.doml.thesis.grad.calc

import at.doml.thesis.grad.internal.AccGrads.LastLayer
import at.doml.thesis.grad.internal.{ AccGrads, LayerGrads, NeuronGrads }
import at.doml.thesis.nn.Layer
import at.doml.thesis.util.collection.sized.Vec
import at.doml.thesis.util.par.Parallel
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should
import scala.collection.immutable.ArraySeq

final class ErrorSpec extends AnyFreeSpec with should.Matchers {

  private implicit object TestParallel extends Parallel {
    override def itemsPerThread: Int = Int.MaxValue
    override def execute[A](tasks: Iterator[() => A]): List[A] = tasks.map(_.apply()).toList
    override def shutdown(): Unit = ()
  }

  "Error" - {

    "should correctly calculate error value" in {
      val layer                    = Layer.random(2, 2, (0.0, 0.0))
      val grads                    = layer.neurons.map(n => NeuronGrads(n.w.map(_ => 0.0), 0.0))
      val accGrads: AccGrads[2, 2] = LastLayer(LayerGrads(layer, grads))

      val inputs = Vec.unsafeWrap[Vec[Double, 2], 2](
        ArraySeq(Vec.unsafeWrap(ArraySeq(1.0, 2.0)), Vec.unsafeWrap(ArraySeq(3.0, 4.0)))
      )
      val targets = Vec.unsafeWrap[Vec[Double, 2], 2](
        ArraySeq(Vec.unsafeWrap(ArraySeq(1.0, 2.0)), Vec.unsafeWrap(ArraySeq(3.0, 4.0)))
      )

      val expectedError = (math.pow(1.0 - 0.5, 2.0) + math.pow(2.0 - 0.5, 2.0) +
        math.pow(3.0 - 0.5, 2.0) + math.pow(4.0 - 0.5, 2.0)) / (2.0 * targets.length)
      val result = Error(targets, inputs, accGrads)

      result shouldBe expectedError
    }
  }
}
