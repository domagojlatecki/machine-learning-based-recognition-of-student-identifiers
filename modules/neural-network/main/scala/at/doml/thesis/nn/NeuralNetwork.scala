package at.doml.thesis.nn

import scala.annotation.tailrec

sealed trait NeuralNetwork[In <: Int, Out <: Int] extends Product with Serializable {

  final def out(in: Vec[Double, In]): Vec[Double, Out] = {

    @tailrec
    def loop[I <: Int, O <: Int](nn: NeuralNetwork[I, O], v: Vec[Double, I]): Vec[Double, O] = {
      nn match {
        case NeuralNetwork.ForwardPass(first, rest) => loop(rest, first.out(v))
        case NeuralNetwork.SingleLayer(layer)       => layer.out(v)
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
    type In  = inputs.type
    type Out = outputs.type

    @tailrec
    def loop(m: Int, mid: List[Int])(acc: NeuralNetwork[m.type, Out]): NeuralNetwork[In, Out] = mid match {
      case h :: t => loop(h, t)(ForwardPass(Layer.random(h, m, wRange), acc))
      case Nil    => ForwardPass(Layer.random(inputs, m, wRange), acc)
    }

    middle.reverse match {
      case h :: t => loop(h, t)(SingleLayer(Layer.random(h, outputs, wRange)))
      case Nil    => SingleLayer(Layer.random(inputs, outputs, wRange))
    }
  }
}
