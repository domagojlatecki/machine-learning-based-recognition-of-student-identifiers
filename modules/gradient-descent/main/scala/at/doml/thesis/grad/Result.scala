package at.doml.thesis.grad

import at.doml.thesis.nn.NeuralNetwork

sealed trait Result[In <: Int, Out <: Int] extends Product with Serializable {
  val nn: NeuralNetwork[In, Out]
}

object Result {

  final case class MaxIterations[In <: Int, Out <: Int](nn: NeuralNetwork[In, Out]) extends Result[In, Out]

  final case class TargetError[In <: Int, Out <: Int](nn: NeuralNetwork[In, Out]) extends Result[In, Out]

  final case class NoTrainingData[In <: Int, Out <: Int](nn: NeuralNetwork[In, Out]) extends Result[In, Out]
}
