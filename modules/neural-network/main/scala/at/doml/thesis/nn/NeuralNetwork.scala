package at.doml.thesis.nn

import scala.annotation.tailrec

sealed trait NeuralNetwork[In <: Int, Out <: Int] extends Product with Serializable {

  final def out(in: Vec[Double, In]): Vec[Double, Out] = {

    @tailrec
    def loop[I <: Int, O <: Int](nn: NeuralNetwork[I, O], v: Vec[Double, I]): Vec[Double, O] = {
      nn match {
        case NeuralNetwork.SingleLayer(layer)       => layer.out(v)
        case NeuralNetwork.ForwardPass(first, rest) => loop(rest, first.out(v))
      }
    }

    loop(this, in)
  }
}

object NeuralNetwork {

  final case class SingleLayer[In <: Int, Out <: Int](layer: Layer[In, Out]) extends NeuralNetwork[In, Out]

  final case class ForwardPass[In <: Int, Mid <: Int, Out <: Int](
    first: Layer[In, Mid],
    rest:  NeuralNetwork[Mid, Out]
  ) extends NeuralNetwork[In, Out]

  def random(
    inputs:  Int,
    middle:  List[Int],
    outputs: Int,
    wRange:  (Double, Double)
  ): NeuralNetwork[inputs.type, outputs.type] = {

    def loop(in: Int, mid: List[Int]): NeuralNetwork[in.type, outputs.type] = mid match {
      case Nil    => SingleLayer(Layer.random(in, outputs, wRange))
      case h :: t => ForwardPass(Layer.random(in, h, wRange), loop(h, t))
    }

    loop(inputs, middle)
  }
}
