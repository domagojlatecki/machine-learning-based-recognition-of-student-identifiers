package at.doml.thesis

import at.doml.thesis.neuralnetwork.{NeuralNetwork, Sample}
import scala.collection.immutable.ArraySeq

object Backpropagation {

  type Batch = List[Sample]

  private def sq(x: Double) = x * x

  private def timed[A](name: String)(f: => A): A = {
    val start = System.nanoTime()
    val out = f
    val end = System.nanoTime()
    println(s"[$name]: ${(end - start) / 1000 / 1000}ms")
    out
  }

  private def calculateError(neuralNetwork: NeuralNetwork, samples: List[Batch], numSamples: Int): Double = timed("calculateError") {
    (for {
      batch <- samples
      sample <- batch
    } yield {
      val networkOutputs = neuralNetwork(sample.input)

      (for (i <- networkOutputs.indices) yield {
        sq(networkOutputs(i) - sample.expectedOutput(i))
      }).sum
    }).sum / (2.0 * numSamples)
  }

  def apply(neuralNetwork: NeuralNetwork, learningStep: Double, trainingSamples: List[Batch],
            maxIterations: Int, targetError: Double): Unit = {
    val numSamples = trainingSamples.map(_.size).sum
    var iteration: Int = 0
    var currentError = calculateError(neuralNetwork, trainingSamples, numSamples)

    println(s"Initial error: $currentError")

    while (iteration < maxIterations && currentError > targetError) {
      for (batch <- trainingSamples) timed("batch loop") {
        var delta: Array[Array[Double]] = null

        for (layerIndex <- neuralNetwork.layers.indices.reverse) {
          if (delta == null) {
            delta = updateLastLayer(batch, neuralNetwork, learningStep, layerIndex)
          } else {
            delta = updateMiddleLayer(batch, neuralNetwork, learningStep, layerIndex, delta)
          }
        }
      }

      iteration += 1
      currentError = calculateError(neuralNetwork, trainingSamples, numSamples)

      println(s"Iteration $iteration error: $currentError")
    }
  }

  private def updateLastLayer(batch: Batch, neuralNetwork: NeuralNetwork, learningStep: Double,
                              layerIndex: Int): Array[Array[Double]] = timed("updateLastLayer") {
    val y: Array[ArraySeq[Double]] = (for (sample <- batch) yield {
      neuralNetwork(sample.input)
    }).toArray
    val t: Array[ArraySeq[Double]] = batch.map(_.expectedOutput).toArray
    val delta: Array[Array[Double]] = new Array(y.length)

    for (i <- delta.indices) {
      delta(i) = new Array(neuralNetwork.getNumNeurons(layerIndex))
    }

    for {
      j <- 0 until neuralNetwork.getNumNeurons(layerIndex)
      i <- 0 until neuralNetwork.getNumWeights(layerIndex, j)
    } {
      val change = (for ((sample, s)<- batch.zipWithIndex) yield {
        delta(s)(j) = y(s)(j) * (1.0 - y(s)(j)) * (t(s)(j) - y(s)(j))
        delta(s)(j) * neuralNetwork.getOutputBeforeNthLayer(sample.input, layerIndex)(i)
      }).sum * learningStep

      neuralNetwork.updateWeight(i, j, layerIndex, change)
    }

    for (j <- 0 until neuralNetwork.getNumNeurons(layerIndex)) {
      val change = (for (s <- batch.indices) yield {
        delta(s)(j)
      }).sum * learningStep

      neuralNetwork.updateWeight(neuralNetwork.getNumWeights(layerIndex, j), j, layerIndex, change)
    }

    delta
  }

  private def updateMiddleLayer(batch: Batch, neuralNetwork: NeuralNetwork, learningStep: Double,
                                layerIndex: Int, previousDelta: Array[Array[Double]]): Array[Array[Double]] = timed("updateMiddleLayer") {
    val y: Array[ArraySeq[Double]] = (for (sample <- batch) yield {
      neuralNetwork.getOutputBeforeNthLayer(sample.input, layerIndex + 1)
    }).toArray
    val delta: Array[Array[Double]] = new Array(y.length)

    for (i <- delta.indices) {
      delta(i) = new Array(neuralNetwork.getNumNeurons(layerIndex))
    }

    for {
      j <- 0 until neuralNetwork.getNumNeurons(layerIndex)
      i <- 0 until neuralNetwork.getNumWeights(layerIndex, j)
    } {
      val change = (for ((sample, s) <- batch.zipWithIndex) yield {
        val nextCorrectionSum = (for (o <- previousDelta(s).indices) yield {
          previousDelta(s)(o) * neuralNetwork.weightAt(j, o, layerIndex + 1)
        }).sum

        delta(s)(j) = y(s)(j) * (1.0 - y(s)(j)) * nextCorrectionSum
        delta(s)(j) * neuralNetwork.getOutputBeforeNthLayer(sample.input, layerIndex)(i)
      }).sum * learningStep

      neuralNetwork.updateWeight(i, j, layerIndex, change)
    }

    for (j <- 0 until neuralNetwork.getNumNeurons(layerIndex)) {
      val change = (for (s <- batch.indices) yield {
        delta(s)(j)
      }).sum * learningStep

      neuralNetwork.updateWeight(neuralNetwork.getNumWeights(layerIndex, j), j, layerIndex, change)
    }

    delta
  }
}
