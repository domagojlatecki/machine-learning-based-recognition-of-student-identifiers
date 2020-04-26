package at.doml.thesis.grad

import at.doml.thesis.grad.NeuralNetworkData.{BackwardPassData, FirstLayerData}
import at.doml.thesis.grad.Result.NoTrainingData
import at.doml.thesis.nn.{Layer, NeuralNetwork, Neuron}
import at.doml.thesis.nn.NeuralNetwork.{ForwardPass, LastLayer}
import at.doml.thesis.util.{Parallel, Vec}
import scala.annotation.tailrec
import scala.collection.immutable.ArraySeq

object GradientCalc {

  private final case class NeuronGrads[In <: Int](w: Vec[Double, In], w0: Double)

  private def calcLayerOutputs[In <: Int, Out <: Int, N <: Int](
    sampleInputs: Vec[Vec[Double, In], N],
    nn:           NeuralNetwork[In, Out]
  )(implicit par: Parallel): NeuralNetworkData[In, Out, N] = {

     @tailrec
     def loop[FI <: Int, I <: Int](
       n:   NeuralNetwork[I, Out],
       ins: Vec[Vec[Double, I], N],
       acc: NeuralNetworkData[FI, I, N]
     ): NeuralNetworkData[FI, Out, N] =
       n match {

         case ForwardPass(first, rest) =>
           val outs = ins.parMap(first.out)
           loop(rest, outs, BackwardPassData(LayerData(first.neurons, ins, outs), acc))

         case LastLayer(layer) =>
           BackwardPassData(LayerData(layer.neurons, ins, ins.parMap(layer.out)), acc)
       }


     nn match {

       case ForwardPass(first, rest) =>
         val outs = sampleInputs.parMap(first.out)
         loop(rest, outs, FirstLayerData(LayerData(first.neurons, sampleInputs, sampleInputs.parMap(first.out))))

       case LastLayer(layer) =>
         FirstLayerData(LayerData(layer.neurons, sampleInputs, sampleInputs.parMap(layer.out)))
     }
   }

  private def calcNextWeights[In <: Int, Out <: Int, N <: Int](
    sampleTargets: Vec[Vec[Double, Out], N],
    nnd:           NeuralNetworkData[In, Out, N],
    step:          Double
  )(implicit par: Parallel): NeuralNetwork[In, Out] = {

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
      layerData.sampleOutputs.mapWithIndex { (outputs, n) =>
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
          val tasks = layerData.sampleInputs.indices.grouped(par.itemsPerThread).map { indices =>
            () => {
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

        var w0 = 0.0

        for (n <- layerData.sampleInputs.indices) {
          w0 += deltas(n)(o)
        }

        NeuronGrads(w, w0)
      }
    }

    def applyGradients[I <: Int, O <: Int, NO <: Int](
      layerData: LayerData[I, O, N],
      grads:     Vec[NeuronGrads[I], O]
    ): Layer[I, O] = {
      Layer(
        layerData.neurons.mapWith(grads) { (neuron, grad) =>
          Neuron(
            neuron.w.mapWith(grad.w)((w, dw) => w + step * dw),
            neuron.w0 + step * grad.w0
          )
        }
      )
    }

    @tailrec
    def loop[I <: Int, O <: Int, NO <: Int, FO <: Int](
      n:           NeuralNetworkData[In, O, N],
      prevNeurons: Vec[Neuron[O], NO],
      prevDeltas:  Vec[Vec[Double, NO], N],
      acc:         NeuralNetwork[O, FO]
    ): NeuralNetwork[In, FO] = n match {

      case BackwardPassData(layerData, previous) =>
        val deltas = calcLayerDeltas(layerData, prevNeurons, prevDeltas)
        val grads = calcGradients(layerData, deltas)
        val layer = applyGradients(layerData, grads)
        loop(previous, layerData.neurons, deltas, ForwardPass(layer, acc))

      case FirstLayerData(layerData) =>
        val deltas = calcLayerDeltas(layerData, prevNeurons, prevDeltas)
        val grads = calcGradients(layerData, deltas)
        val layer = applyGradients(layerData, grads)
        ForwardPass(layer, acc)
    }

    nnd match {

      case BackwardPassData(layerData, previous) =>
        val deltas = calcLastLayerDeltas(layerData)
        val grads = calcGradients(layerData, deltas)
        val layer = applyGradients(layerData, grads)
        loop(previous, layerData.neurons, deltas, LastLayer(layer))

      case FirstLayerData(layerData) =>
        val deltas = calcLastLayerDeltas(layerData)
        val grads = calcGradients(layerData, deltas)
        val layer = applyGradients(layerData, grads)
        LastLayer(layer)
    }
  }

  private def calcError[In <: Int, Out <: Int, N <: Int](
    targets: Vec[Vec[Double, Out], N],
    inputs:  Vec[Vec[Double, In], N],
    nn:      NeuralNetwork[In, Out]
  )(implicit par: Parallel): Double = {
    val outputs = inputs.map(nn.out)
    val tasks = targets.indices.grouped(par.itemsPerThread).map { indices =>
      () => {
        var sum = 0.0

        for (n <- indices) {
          val target = targets(n)
          val output = outputs(n)

          for (o <- target.indices) {
            sum += math.pow(target(o) - output(o), 2.0)
          }
        }

        sum
      }
    }

    par.execute(tasks).sum / (2.0 * targets.length)
  }

  def optimize[In <: Int, Out <: Int, N <: Int](
    nn: NeuralNetwork[In, Out]
  )(
    samples:     Vec[Sample[In, Out], N],
    step:        Double,
    batchSize:   BatchSize,
    maxIters:    Int,
    targetError: Double
  )(implicit par: Parallel): Result[In, Out] = {
    val inputs = samples.map(_.input)
    val targets = samples.map(_.target)

    val numOfBatchSamples = batchSize match {
      case BatchSize.of(v) => v
      case BatchSize.all   => samples.length
    }

    val inputBatches = inputs.underlying.grouped(numOfBatchSamples).toList
    val targetBatches = targets.underlying.grouped(numOfBatchSamples).toList

    type Batch = (ArraySeq[Vec[Double, In]], ArraySeq[Vec[Double, Out]])
    val batches: List[Batch] = inputBatches.zip(targetBatches)

    @tailrec
    def batchLoop(n: NeuralNetwork[In, Out], batch: Batch, rest: List[Batch]): NeuralNetwork[In, Out] = {
      val len = batch._1.length
      val input: Vec[Vec[Double, In], len.type] = Vec.unsafeWrap(batch._1)
      val target: Vec[Vec[Double, Out], len.type] = Vec.unsafeWrap(batch._2)
      val adjustedStep = step / len
      val nnd = calcLayerOutputs(input, n)
      val next = calcNextWeights(target, nnd, adjustedStep)

      rest match {
        case Nil    => next
        case h :: t => batchLoop(next, h, t)
      }
    }

    @tailrec
    def loop(n: NeuralNetwork[In, Out], iter: Int, err: Double): Result[In, Out] = {
      if (err <= targetError) {
        Result.TargetError(n)
      } else if (iter > maxIters) {
        Result.MaxIterations(n)
      } else {
        batches match {
          case Nil    => NoTrainingData(n)
          case h :: t =>
            val next = batchLoop(n, h, t)
            val error = calcError(targets, inputs, next)

            println(s"Iteration $iter error: $error")

            loop(next, iter + 1, error)
        }
      }
    }

    val error = calcError(targets, inputs, nn)
    println(s"Initial error: $error")

    loop(nn, 1, error)
  }
}
