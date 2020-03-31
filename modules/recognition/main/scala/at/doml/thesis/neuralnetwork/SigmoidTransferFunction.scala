package at.doml.thesis.neuralnetwork

object SigmoidTransferFunction extends TransferFunction {

  override def apply(net: Double): Double = 1.0 / (1.0 + Math.exp(-net))
}
