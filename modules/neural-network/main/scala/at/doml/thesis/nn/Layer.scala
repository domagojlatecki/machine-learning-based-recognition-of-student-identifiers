package at.doml.thesis.nn

import Vec.Size

final case class Layer[In <: Size, Out <: Size](neurons: Vec[Neuron[In], Out]) {

  def out(in: Vec[Double, In]): Vec[Double, Out] =
    neurons.map(_.out(in))
}

object Layer {

  def random[In <: Size, Out <: Size](inputs: In, outputs: Out, wRange: (Double, Double)): Layer[In, Out] =
    Layer(Vec.fill(outputs)(Neuron.random(inputs, wRange)))
}
