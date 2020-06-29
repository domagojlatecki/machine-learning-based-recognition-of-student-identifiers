package at.doml.thesis.grad.internal

import at.doml.thesis.nn.Layer
import at.doml.thesis.util.collection.sized.Vec
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should

class LayerGradsSpec extends AnyFreeSpec with should.Matchers {

  "LayerGrads" - {

    "should produce same output as wrapped Layer for some input Vec" in {
      (0 until 100).foreach { _ =>
        val input      = Vec.fill(10)(math.random())
        val layer      = Layer.random(10, 5, (-0.5, 0.5))
        val grads      = layer.neurons.map(n => NeuronGrads(n.w.map(_ => 0.0), 0.0))
        val layerGrads = LayerGrads(layer, grads)

        layerGrads.out(input).underlying shouldBe layer.out(input).underlying
      }
    }
  }
}
