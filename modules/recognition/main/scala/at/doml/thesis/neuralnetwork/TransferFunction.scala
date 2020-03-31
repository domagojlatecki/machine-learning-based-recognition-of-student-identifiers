package at.doml.thesis.neuralnetwork

trait TransferFunction {

  def apply(net: Double): Double
}
