package at.doml.thesis.grad

import at.doml.thesis.nn.Vec

final case class Sample[In <: Int, Out <: Int](input: Vec[Double, In], target: Vec[Double, Out])
