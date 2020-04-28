package at.doml.thesis.grad.internal

import at.doml.thesis.nn.Neuron
import at.doml.thesis.util.Vec

private[grad] final case class LayerData[In <: Int, Out <: Int, N <: Int](
  neurons:       Vec[Neuron[In], Out],
  sampleInputs:  Vec[Vec[Double, In], N],
  sampleOutputs: Vec[Vec[Double, Out], N],
  accGrads:      Vec[NeuronGrads[In], Out]
)
