package at.doml.thesis.grad.internal

import at.doml.thesis.util.collection.sized.Vec

private[grad] final case class NeuronGrads[In <: Int](w: Vec[Double, In], w0: Double)
