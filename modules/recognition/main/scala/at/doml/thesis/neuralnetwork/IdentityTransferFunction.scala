package at.doml.thesis.neuralnetwork

object IdentityTransferFunction extends TransferFunction {

  override def apply(net: Double): Double = net
}
