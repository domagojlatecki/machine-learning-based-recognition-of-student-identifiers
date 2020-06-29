package at.doml.thesis.grad

import at.doml.thesis.grad.internal.{ LayerGrads, NeuronGrads }
import at.doml.thesis.nn.Layer
import at.doml.thesis.util.collection.sized.Vec
import scala.collection.immutable.ArraySeq

object LayerGradsUtil {

  implicit final class LayerOps[I <: Int, O <: Int](l: Layer[I, O]) {
    def weights: ArraySeq[Double] = l.neurons.underlying.flatMap(n => n.w.underlying :+ n.w0)
  }

  implicit final class VecOfNeuronGradsOps[I <: Int, O <: Int](v: Vec[NeuronGrads[I], O]) {
    def weights: ArraySeq[Double] = v.underlying.flatMap(n => n.w.underlying :+ n.w0)
  }

  def randomLayerGrands(in: Int, out: Int, gradValue: => Double): LayerGrads[in.type, out.type] = {
    val layer = Layer.random(in, out, (-0.5, 0.5))
    val grads = layer.neurons.map(n => NeuronGrads(n.w.map(_ => gradValue), gradValue))
    LayerGrads(layer, grads)
  }
}
