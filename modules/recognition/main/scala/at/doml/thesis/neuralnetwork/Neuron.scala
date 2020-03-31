package at.doml.thesis.neuralnetwork

import scala.collection.immutable.ArraySeq
import scala.util.Random

class Neuron(private val transferFunction: TransferFunction, val numInputs: Int) {

  private val weights: Array[Double] = (for (_ <- 0 until this.numInputs + 1) yield {
    Random.nextDouble()
  }).toArray

  def weightAt(index: Int): Double = this.weights(index)

  def update(index: Int, change: Double): Unit = {
    this.weights(index) += change
  }

  def apply(inputs: ArraySeq[Double]): Double = {
    if (inputs.length != this.numInputs) {
      throw new IllegalArgumentException(
        s"invalid number of inputs: expected[${this.numInputs}], actual[${inputs.length}]"
      )
    }

    this.transferFunction(
      (for (i <- inputs.indices) yield {
        this.weights(i) * inputs(i)
      }).sum + this.weights(this.numInputs)
    )
  }
}
