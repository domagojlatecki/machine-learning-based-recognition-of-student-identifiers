package at.doml.thesis.grad

import at.doml.thesis.grad.GradientDescentOptimizer.{NeuronGradients, SampleWithLayerOutputs}
import at.doml.thesis.nn.NeuralNetwork.{ForwardPass, LastLayer}
import at.doml.thesis.nn.{Layer, NeuralNetwork, Neuron}
import at.doml.thesis.util.Vec
import scala.annotation.tailrec
import scala.collection.immutable.ArraySeq

final class GradientDescentOptimizer[In <: Int, Out <: Int](private var nn: NeuralNetwork[In, Out]) {

  private val NumLayers: Int = {

    @tailrec
    def loop(n: NeuralNetwork[_, _], acc: Int): Int = {
      n match {
        case NeuralNetwork.ForwardPass(_, rest) => loop(rest, acc + 1)
        case NeuralNetwork.LastLayer(_)         => acc + 1
      }
    }

    loop(nn, 0)
  }

  private type NumLayers = NumLayers.type
  private type ReversedNeurons = Vec[Vec[Neuron[_], _], NumLayers]
  private type ReversedDeltas = Vec[Vec[Double, _], NumLayers]
  private type ReversedGradients = Vec[Vec[NeuronGradients, _], NumLayers]
  private type SampleOutputs = SampleWithLayerOutputs[In, Out, NumLayers]

  private def optStep[N <: Int](samples: Vec[Sample[In, Out], N], step: Double): Unit = {
    val adjustedStep = step / samples.length

    def calcLayerOutputs(): Vec[SampleOutputs, N] = {

      @tailrec
      def loop[I <: Int, O <: Int](
        n: NeuralNetwork[I, O],
        v: Vec[Double, I],
        acc: List[Vec[Double, _]]
      ): List[Vec[Double, _]] = {
        n match {

          case NeuralNetwork.ForwardPass(first, rest) =>
            val o = first.out(v)
            loop(rest, o, o :: acc)

          case NeuralNetwork.LastLayer(layer) =>
            layer.out(v) :: acc

        }
      }

      samples.map(s => SampleWithLayerOutputs(s, Vec.unsafeFromIterable(loop(nn, s.input, Nil))))
    }

    def extractNeurons(): ReversedNeurons = {

      @tailrec
      def loop(n: NeuralNetwork[_, _], acc: List[Vec[Neuron[_], _]]): List[Vec[Neuron[_], _]] = {
        n match {
          case NeuralNetwork.ForwardPass(first, rest) => loop(rest, first.neurons :: acc)
          case NeuralNetwork.LastLayer(layer)         => layer.neurons :: acc
        }
      }

      Vec.unsafeFromIterable(loop(nn, Nil))
    }

    def calcDeltas(sampleOutputs: Vec[SampleOutputs, N], allNeurons: ReversedNeurons): Vec[ReversedDeltas, N] = {
      sampleOutputs.map { s =>
        val layerOutputs = s.reversedLayerOutputs.underlying
        val target = s.sample.target
        val initialDelta = layerOutputs(0).mapWithIndex((y, i) => y * (1.0 - y) * (target(i) - y)).underlying

        Vec.iterate((1, initialDelta), NumLayers) {
          case (layerIndex, previousDelta) =>
            val layerOut = layerOutputs(layerIndex)
            val prevLayerNeurons = allNeurons(layerIndex - 1)
            val deltas = layerOut.mapWithIndex { (y, i) =>
              var sum = 0.0
              for (j <- prevLayerNeurons.indices) {
                sum += previousDelta(j) * prevLayerNeurons(j).w(i)
              }
              y * (1.0 - y) * sum
            }.underlying

            (layerIndex + 1, deltas)
        }.map(t => Vec.unsafeFromIterable(t._2))
      }
    }

    def calcGradients(
      sampleOutputs: Vec[SampleOutputs, N],
      deltas: Vec[ReversedDeltas, N],
      allNeurons: ReversedNeurons
    ): ReversedGradients = {
      allNeurons.mapWithIndex { (layerNeurons, layerIndex) =>
        layerNeurons.mapWithIndex { (neuron, neuronIndex) =>
          val wGrads = neuron.w.mapWithIndex { (_, wIndex) =>
            var sum = 0.0

            for (n <- sampleOutputs.indices) {
              val y = sampleOutputs(n).getInputsForLayer(layerIndex)(wIndex)
              val d = deltas(n)(layerIndex)(neuronIndex)
              sum += d * y
            }

            sum
          }

          var w0Grad = 0.0

          for (n <- sampleOutputs.indices) {
            w0Grad += deltas(n)(layerIndex)(neuronIndex)
          }

          NeuronGradients(wGrads.underlying, w0Grad)
        }
      }
    }

    def applyGradients(gradients: ReversedGradients, allNeurons: ReversedNeurons): Unit = {
      val adjustedLayers = gradients.mapWithIndex { (grads, layerIndex) =>
        val neurons = allNeurons(layerIndex)

        val adjustedNeurons = neurons.mapWithIndex { (neuron, neuronIndex) =>
          val grad = grads(neuronIndex)

          val w = neuron.w.mapWithIndex { (w, wIndex) => w + adjustedStep * grad.wGrads(wIndex) }

          val w0 = neuron.w0 + adjustedStep * grad.w0Grad

          Neuron[-1](w.asInstanceOf[Vec[Double, -1]], w0)
        }

        Layer[-1, -1](adjustedNeurons.asInstanceOf[Vec[Neuron[-1], -1]])
      }

      @tailrec
      def loop(l: List[Layer[-1, -1]], acc: NeuralNetwork[-1, -1]): NeuralNetwork[-1, -1] = l match {
        case h :: t => loop(t, ForwardPass(h, acc))
        case Nil    => acc
      }

      val adjustedNetwork = adjustedLayers.underlying.toList match {
        case h :: t => loop(t, LastLayer(h))
      }

      nn = adjustedNetwork.asInstanceOf[NeuralNetwork[In, Out]]
    }

    val layerOutputs: Vec[SampleOutputs, N] = calcLayerOutputs()
    val neurons: ReversedNeurons = extractNeurons()
    val deltas: Vec[ReversedDeltas, N] = calcDeltas(layerOutputs, neurons)
    val gradients: ReversedGradients = calcGradients(layerOutputs, deltas, neurons)

    applyGradients(gradients, neurons)
  }

  def optimize[N <: Int](
    samples:     Vec[Sample[In, Out], N],
    step:        Double,
    batchSize:   Int,
    maxIters:    Int,
    targetError: Double
  ): Unit = {

    def calcError[S <: Int](batch: Vec[Sample[In, Out], S]): Double = {
      val so = batch.underlying
      var sum = 0.0

      for (i <- so.indices) {
        val s = so(i)
        val output = nn.out(s.input).underlying
        val target = s.target.underlying

        for (j <- output.indices) {
          sum += math.pow(target(j) - output(j), 2.0)
        }
      }

      sum / (2.0 + samples.length)
    }

    var error = calcError(samples)
    var iter = 1

    println(s"Initial error: $error")

    val batches = samples.underlying.grouped(batchSize).map(Vec.unsafeFromIterable[Sample[In, Out], -1]).toList

    while (iter <= maxIters && error > targetError) {
      for (batch <- batches) {
        optStep(batch, step)
      }

      error = calcError(samples)
      println(s"Iteration $iter error: $error")
      iter += 1
    }
  }

  def result: NeuralNetwork[In, Out] = nn
}

object GradientDescentOptimizer {

  private final case class SampleWithLayerOutputs[In <: Int, Out <: Int, NumLayers <: Int](
    sample: Sample[In, Out],
    reversedLayerOutputs: Vec[Vec[Double, _], NumLayers]
  ) {

    def getInputsForLayer(index: Int): Vec[Double, _] = {
      if (index + 1 == reversedLayerOutputs.length) {
        sample.input
      } else {
        reversedLayerOutputs(index + 1)
      }
    }
  }

  private final case class NeuronGradients(wGrads: ArraySeq[Double], w0Grad: Double)
}
