package at.doml.thesis.neuralnetwork

import scala.collection.immutable.ArraySeq

class NeuralNetwork(val layers: List[Layer]) {

  def apply(inputs: ArraySeq[Double]): ArraySeq[Double] = {
    var in = inputs

    for (layer <- layers) {
      in = layer(in)
    }

    in
  }

  def getOutputBeforeNthLayer(inputs: ArraySeq[Double], n: Int): ArraySeq[Double] = {
    if (n == 0) {
      return inputs
    }

    var in = inputs

    for (layer <- layers.take(n)) {
      in = layer(in)
    }

    in
  }

  def weightAt(index: Int, neuronIndex: Int, layerIndex: Int): Double = this.layers(layerIndex)
    .weightAt(index, neuronIndex)

  def getNumNeurons(layerIndex: Int): Int = this.layers(layerIndex).numNeurons

  def getNumWeights(layerIndex: Int, neuronIndex: Int): Int = this.layers(layerIndex).getNumWeights(neuronIndex)

  def updateWeight(index: Int, neuronIndex: Int, layerIndex: Int, change: Double): Unit = this.layers(layerIndex)
    .updateWeight(index, neuronIndex, change)
}
