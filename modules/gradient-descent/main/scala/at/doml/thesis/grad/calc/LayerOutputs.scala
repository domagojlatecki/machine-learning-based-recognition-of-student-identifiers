package at.doml.thesis.grad.calc

import at.doml.thesis.grad.internal.AccGrads.{ ForwardPass, LastLayer }
import at.doml.thesis.grad.internal.NeuralNetworkData.{ BackwardPassData, FirstLayerData }
import at.doml.thesis.grad.internal.{ AccGrads, LayerData, NeuralNetworkData }
import at.doml.thesis.util.collection.sized.Vec
import at.doml.thesis.util.par.Parallel
import scala.annotation.tailrec

private[grad] object LayerOutputs {

  // format: off
  def apply[In <: Int, Out <: Int, N <: Int](
    sampleInputs: Vec[Vec[Double, In], N],
    nn:           AccGrads[In, Out]
  )(implicit par: Parallel): NeuralNetworkData[In, Out, N] = {
    // format: on

    @tailrec
    def loop[FI <: Int, I <: Int](
      n:   AccGrads[I, Out],
      ins: Vec[Vec[Double, I], N],
      acc: NeuralNetworkData[FI, I, N]
    ): NeuralNetworkData[FI, Out, N] =
      n match {

        case ForwardPass(first, rest) =>
          val outs = ins.parMap(first.out)
          loop(rest, outs, BackwardPassData(LayerData(first.layer.neurons, ins, outs, first.accGrads), acc))

        case LastLayer(lg) =>
          BackwardPassData(LayerData(lg.layer.neurons, ins, ins.parMap(lg.layer.out), lg.accGrads), acc)
      }

    nn match {

      case ForwardPass(first, rest) =>
        val outs      = sampleInputs.parMap(first.out)
        val layerData = LayerData(first.layer.neurons, sampleInputs, outs, first.accGrads)
        loop(rest, outs, FirstLayerData(layerData))

      case LastLayer(lg) =>
        FirstLayerData(LayerData(lg.layer.neurons, sampleInputs, sampleInputs.parMap(lg.layer.out), lg.accGrads))
    }
  }
}
