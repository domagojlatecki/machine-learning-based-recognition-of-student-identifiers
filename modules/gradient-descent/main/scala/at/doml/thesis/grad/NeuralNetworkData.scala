package at.doml.thesis.grad

private[grad] sealed trait NeuralNetworkData[In <: Int, Out <: Int, N <: Int] extends Product with Serializable

private[grad] object NeuralNetworkData {

  final case class FirstLayerData[In <: Int, Out <: Int, N <: Int](layerData: LayerData[In, Out, N])
    extends NeuralNetworkData[In, Out, N]

  final case class BackwardPassData[In <: Int, Mid <: Int, Out <: Int, N <: Int](
    layerData: LayerData[Mid, Out, N],
    previous:  NeuralNetworkData[In, Mid, N]
  ) extends NeuralNetworkData[In, Out, N]
}
