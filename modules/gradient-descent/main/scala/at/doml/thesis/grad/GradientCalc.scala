package at.doml.thesis.grad

import at.doml.thesis.grad.Result.NoTrainingData
import at.doml.thesis.grad.internal.{AccGrads, LayerData, LayerGrads, NeuralNetworkData, NeuronGrads}
import at.doml.thesis.grad.internal.AccGrads.{ForwardPass, LastLayer}
import at.doml.thesis.grad.internal.NeuralNetworkData.{BackwardPassData, FirstLayerData}
import at.doml.thesis.nn.{Layer, NeuralNetwork, Neuron}
import at.doml.thesis.util.{Parallel, Vec}
import scala.annotation.tailrec
import scala.collection.immutable.ArraySeq

// TODO refactor
object GradientCalc {

  private def calcLayerOutputs[In <: Int, Out <: Int, N <: Int](
    sampleInputs: Vec[Vec[Double, In], N],
    nn:           AccGrads[In, Out]
  )(implicit par: Parallel): NeuralNetworkData[In, Out, N] = {

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
         val outs = sampleInputs.parMap(first.out)
         val layerData = LayerData(first.layer.neurons, sampleInputs, outs, first.accGrads)
         loop(rest, outs, FirstLayerData(layerData))

       case LastLayer(lg) =>
         FirstLayerData(LayerData(lg.layer.neurons, sampleInputs, sampleInputs.parMap(lg.layer.out), lg.accGrads))
     }
   }

  private def calcNextWeights[In <: Int, Out <: Int, N <: Int](
    sampleTargets:    Vec[Vec[Double, Out], N],
    nnd:              NeuralNetworkData[In, Out, N],
    step:             Double,
    gradCoeff:        Double,
    gradDecay:        Double
  )(implicit par: Parallel): AccGrads[In, Out] = {

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

        val w0Tasks = layerData.sampleInputs.indices.grouped(par.itemsPerThread).map { indices =>
          () => {
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
      // TODO use all grad history
      LayerGrads(
        Layer(
          layerData.neurons.mapWith(grads, layerData.accGrads) { (neuron, grad, accGrads) =>
            Neuron(
              neuron.w.mapWith(grad.w, accGrads.w)((w, dw, acc) => w + gradCoeff * step * dw + gradDecay * step * acc),
              neuron.w0 + gradCoeff * step * grad.w0 + gradDecay * step * accGrads.w0
            )
          }
        ),
        grads
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
    nn:      AccGrads[In, Out]
  )(implicit par: Parallel): Double = {
    val outputs = inputs.parMap(nn.out)
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
    inertia:     Double,
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
    def batchLoop(n: AccGrads[In, Out], batch: Batch, rest: List[Batch]): AccGrads[In, Out] = {
      val len = batch._1.length
      val input: Vec[Vec[Double, In], len.type] = Vec.unsafeWrap(batch._1)
      val target: Vec[Vec[Double, Out], len.type] = Vec.unsafeWrap(batch._2)
      val adjustedStep = step / len
      val nnd = calcLayerOutputs(input, n)
      val next = calcNextWeights(target, nnd, adjustedStep, 1.0 - inertia, inertia)

      rest match {
        case Nil    => next
        case h :: t => batchLoop(next, h, t)
      }
    }

    @tailrec
    def loop(n: AccGrads[In, Out], iter: Int, err: Double): Result[In, Out] = {
      if (err <= targetError) {
        Result.TargetError(AccGrads.unwrap(n))
      } else if (iter > maxIters) {
        Result.MaxIterations(AccGrads.unwrap(n))
      } else {
        batches match {
          case Nil    => NoTrainingData(AccGrads.unwrap(n))
          case h :: t =>
            val next = batchLoop(n, h, t)
            val error = calcError(targets, inputs, next)
            val color = if (err > error) "\u001B[32m" else if (err < error) "\u001B[31m" else "\u001B[33m"

            println(s"${color}Iteration $iter error: $error\u001B[0m")

            loop(next, iter + 1, error)
        }
      }
    }

    val acc = AccGrads.wrap(nn) // TODO initialize to iteration 1 gradients
    val error = calcError(targets, inputs, acc)
    println(s"Initial error: $error")

    loop(acc, 1, error)
  }
}
