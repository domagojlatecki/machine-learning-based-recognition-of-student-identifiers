package at.doml.thesis.nn

final case class Layer[In <: Int, Out <: Int](neurons: Vec[Neuron[In], Out]) {

  def out(in: Vec[Double, In]): Vec[Double, Out] =
    neurons.map(_.out(in))
}

object Layer {

  def random(inputs: Int, outputs: Int, wRange: (Double, Double)): Layer[inputs.type, outputs.type] =
    Layer(Vec.fill(outputs)(Neuron.random(inputs, wRange)))
}
