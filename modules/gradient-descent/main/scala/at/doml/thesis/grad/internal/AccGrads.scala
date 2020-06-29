package at.doml.thesis.grad.internal

import at.doml.thesis.grad.internal.AccGrads.{ ForwardPass, LastLayer }
import at.doml.thesis.grad.internal.NeuralNetworkData.{ BackwardPassData, FirstLayerData }
import at.doml.thesis.nn.{ Layer, NeuralNetwork }
import at.doml.thesis.util.collection.sized.Vec
import scala.annotation.tailrec

private[grad] sealed trait AccGrads[In <: Int, Out <: Int] extends Product with Serializable {

  final def out(in: Vec[Double, In]): Vec[Double, Out] = {

    @tailrec
    def loop[I <: Int, O <: Int](gs: AccGrads[I, O], v: Vec[Double, I]): Vec[Double, O] = gs match {
      case ForwardPass(first, rest) => loop(rest, first.out(v))
      case LastLayer(layer)         => layer.out(v)
    }

    loop(this, in)
  }
}

private[grad] object AccGrads {

  final case class LastLayer[In <: Int, Out <: Int](layer: LayerGrads[In, Out]) extends AccGrads[In, Out]

  final case class ForwardPass[In <: Int, Mid <: Int, Out <: Int](first: LayerGrads[In, Mid], rest: AccGrads[Mid, Out])
    extends AccGrads[In, Out]

  def wrap[In <: Int, Out <: Int](neuralNetwork: NeuralNetwork[In, Out]): AccGrads[In, Out] = {

    def grads[I <: Int, O <: Int](l: Layer[I, O]): Vec[NeuronGrads[I], O] =
      l.neurons.map(n => NeuronGrads(n.w.map(_ => 0.0), 0.0))

    @tailrec
    def nn2nndLoop[FI <: Int, I <: Int](
      n:   NeuralNetwork[I, Out],
      acc: NeuralNetworkData[FI, I, 0]
    ): NeuralNetworkData[FI, Out, 0] =
      n match {

        case NeuralNetwork.ForwardPass(first, rest) =>
          nn2nndLoop(rest, BackwardPassData(LayerData(first.neurons, Vec.empty, Vec.empty, grads(first)), acc))

        case NeuralNetwork.LastLayer(layer) =>
          BackwardPassData(LayerData(layer.neurons, Vec.empty, Vec.empty, grads(layer)), acc)
      }

    val neuralNetworkData = neuralNetwork match {

      case NeuralNetwork.ForwardPass(first, rest) =>
        nn2nndLoop(rest, FirstLayerData(LayerData(first.neurons, Vec.empty, Vec.empty, grads(first))))

      case NeuralNetwork.LastLayer(layer) =>
        FirstLayerData(LayerData(layer.neurons, Vec.empty, Vec.empty, grads(layer)))
    }

    @tailrec
    def nnd2accLoop[O <: Int, FO <: Int](n: NeuralNetworkData[In, O, 0], acc: AccGrads[O, FO]): AccGrads[In, FO] =
      n match {

        case BackwardPassData(layerData, previous) =>
          nnd2accLoop(previous, ForwardPass(LayerGrads(Layer(layerData.neurons), layerData.accGrads), acc))

        case FirstLayerData(layerData) =>
          ForwardPass(LayerGrads(Layer(layerData.neurons), layerData.accGrads), acc)
      }

    neuralNetworkData match {

      case BackwardPassData(layerData, previous) =>
        nnd2accLoop(previous, LastLayer(LayerGrads(Layer(layerData.neurons), layerData.accGrads)))

      case FirstLayerData(layerData) =>
        LastLayer(LayerGrads(Layer(layerData.neurons), layerData.accGrads))
    }
  }

  def unwrap[In <: Int, Out <: Int](accGrads: AccGrads[In, Out]): NeuralNetwork[In, Out] = {

    @tailrec
    def acc2nndLoop[FI <: Int, I <: Int](
      n:   AccGrads[I, Out],
      acc: NeuralNetworkData[FI, I, 0]
    ): NeuralNetworkData[FI, Out, 0] =
      n match {

        case ForwardPass(first, rest) =>
          acc2nndLoop(rest, BackwardPassData(LayerData(first.layer.neurons, Vec.empty, Vec.empty, first.accGrads), acc))

        case LastLayer(lg) =>
          BackwardPassData(LayerData(lg.layer.neurons, Vec.empty, Vec.empty, lg.accGrads), acc)
      }

    val neuralNetworkData = accGrads match {

      case ForwardPass(first, rest) =>
        acc2nndLoop(rest, FirstLayerData(LayerData(first.layer.neurons, Vec.empty, Vec.empty, first.accGrads)))

      case LastLayer(lg) =>
        FirstLayerData(LayerData(lg.layer.neurons, Vec.empty, Vec.empty, lg.accGrads))
    }

    @tailrec
    def nnd2nnLoop[O <: Int, FO <: Int](
      n:   NeuralNetworkData[In, O, 0],
      acc: NeuralNetwork[O, FO]
    ): NeuralNetwork[In, FO] = n match {

      case BackwardPassData(layerData, previous) =>
        nnd2nnLoop(previous, NeuralNetwork.ForwardPass(Layer(layerData.neurons), acc))

      case FirstLayerData(layerData) =>
        NeuralNetwork.ForwardPass(Layer(layerData.neurons), acc)
    }

    neuralNetworkData match {

      case BackwardPassData(layerData, previous) =>
        nnd2nnLoop(previous, NeuralNetwork.LastLayer(Layer(layerData.neurons)))

      case FirstLayerData(layerData) =>
        NeuralNetwork.LastLayer(Layer(layerData.neurons))
    }
  }
}
