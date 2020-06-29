package at.doml.thesis.grad.calc

import at.doml.thesis.grad.internal.AccGrads.{ ForwardPass, LastLayer }
import at.doml.thesis.grad.internal.NeuralNetworkData.{ BackwardPassData, FirstLayerData }
import at.doml.thesis.grad.internal.{ AccGrads, LayerData, LayerGrads, NeuralNetworkData, NeuronGrads }
import at.doml.thesis.nn.{ Layer, Neuron }
import at.doml.thesis.util.collection.sized.Vec
import at.doml.thesis.util.par.Parallel
import scala.annotation.tailrec

private[grad] object NextWeights {

  def apply[In <: Int, Out <: Int, N <: Int](
    sampleTargets: Vec[Vec[Double, Out], N],
    nnd:           NeuralNetworkData[In, Out, N],
    step:          Double,
    gradCoeff:     Double,
    gradDecay:     Double
  )(implicit par:  Parallel): AccGrads[In, Out] = {

    def calcLastLayerDeltas[I <: Int](layerData: LayerData[I, Out, N]): Vec[Vec[Double, Out], N] = {
      layerData.sampleOutputs.parMapWith(sampleTargets) { (outputs, targets) =>
        outputs.mapWith(targets)((y, t) => y * (1.0 - y) * (t - y))
      }
    }

    def calcLayerDeltas[I <: Int, O <: Int, NO <: Int](
      layerData:   LayerData[I, O, N],
      prevNeurons: Vec[Neuron[O], NO],
      prevDeltas:  Vec[Vec[Double, NO], N]
    ): Vec[Vec[Double, O], N] = {
      layerData.sampleOutputs.parMapWithIndex { (outputs, n) =>
        outputs.mapWithIndex { (y, o) =>
          var sum = 0.0

          for (no <- prevNeurons.indices) {
            sum += prevDeltas(n)(no) * prevNeurons(no).w(o)
          }

          y * (1.0 - y) * sum
        }
      }
    }

    def calcGradients[I <: Int, O <: Int](
      layerData: LayerData[I, O, N],
      deltas:    Vec[Vec[Double, O], N]
    ): Vec[NeuronGrads[I], O] = {
      layerData.neurons.mapWithIndex { (neuron, o) =>
        val w = neuron.w.mapWithIndex { (_, i) =>
          val tasks = layerData.sampleInputs.indices.grouped(par.itemsPerThread).map { indices => () =>
            {
              var sum = 0.0

              for (n <- indices) {
                val y = layerData.sampleInputs(n)(i)
                val d = deltas(n)(o)
                sum += d * y
              }

              sum
            }
          }

          par.execute(tasks).sum
        }

        val w0Tasks = layerData.sampleInputs.indices.grouped(par.itemsPerThread).map { indices => () =>
          {
            var sum = 0.0

            for (n <- indices) {
              sum += deltas(n)(o)
            }

            sum
          }
        }

        val w0 = par.execute(w0Tasks).sum

        NeuronGrads(w, w0)
      }
    }

    def applyGradients[I <: Int, O <: Int, NO <: Int](
      layerData: LayerData[I, O, N],
      grads:     Vec[NeuronGrads[I], O]
    ): LayerGrads[I, O] = {
      val adjustments = grads.mapWith(layerData.accGrads) { (grad, accGrads) =>
        NeuronGrads(
          grad.w.mapWith(accGrads.w)((dw, acc) => gradCoeff * step * dw + gradDecay * acc),
          gradCoeff * step * grad.w0 + gradDecay * accGrads.w0
        )
      }
      LayerGrads(
        Layer(layerData.neurons.mapWith(adjustments) { (neuron, adjustment) =>
          Neuron(neuron.w.mapWith(adjustment.w)((w, dw) => w + dw), neuron.w0 + adjustment.w0)
        }),
        adjustments
      )
    }

    @tailrec
    def loop[I <: Int, O <: Int, NO <: Int, FO <: Int](
      n:           NeuralNetworkData[In, O, N],
      prevNeurons: Vec[Neuron[O], NO],
      prevDeltas:  Vec[Vec[Double, NO], N],
      acc:         AccGrads[O, FO]
    ): AccGrads[In, FO] = n match {

      case BackwardPassData(layerData, previous) =>
        val deltas = calcLayerDeltas(layerData, prevNeurons, prevDeltas)
        val grads  = calcGradients(layerData, deltas)
        val layer  = applyGradients(layerData, grads)
        loop(previous, layerData.neurons, deltas, ForwardPass(layer, acc))

      case FirstLayerData(layerData) =>
        val deltas = calcLayerDeltas(layerData, prevNeurons, prevDeltas)
        val grads  = calcGradients(layerData, deltas)
        val layer  = applyGradients(layerData, grads)
        ForwardPass(layer, acc)
    }

    nnd match {

      case BackwardPassData(layerData, previous) =>
        val deltas = calcLastLayerDeltas(layerData)
        val grads  = calcGradients(layerData, deltas)
        val layer  = applyGradients(layerData, grads)
        loop(previous, layerData.neurons, deltas, LastLayer(layer))

      case FirstLayerData(layerData) =>
        val deltas = calcLastLayerDeltas(layerData)
        val grads  = calcGradients(layerData, deltas)
        val layer  = applyGradients(layerData, grads)
        LastLayer(layer)
    }
  }
}
