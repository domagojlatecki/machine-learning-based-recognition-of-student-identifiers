package at.doml.thesis.grad.internal

import at.doml.thesis.nn.Layer
import at.doml.thesis.util.collection.sized.Vec

private[grad] final case class LayerGrads[In <: Int, Out <: Int](
  layer:    Layer[In, Out],
  accGrads: Vec[NeuronGrads[In], Out]
) {

  @inline
  def out(in: Vec[Double, In]): Vec[Double, Out] =
    layer.out(in)
}
