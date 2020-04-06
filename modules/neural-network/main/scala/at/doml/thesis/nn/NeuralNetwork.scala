package at.doml.thesis.nn

import at.doml.thesis.nn.Vec.Size
import scala.annotation.tailrec

sealed trait NeuralNetwork[In <: Size, Out <: Size] extends Product with Serializable {

  final def out(in: Vec[Double, In]): Vec[Double, Out] = {

    @tailrec
    def loop[I <: Size, O <: Size](nn: NeuralNetwork[I, O], v: Vec[Double, I]): Vec[Double, O] = {
      nn match {
        case NeuralNetwork.SingleLayer(layer)       => layer.out(v)
        case NeuralNetwork.ForwardPass(first, rest) => loop(rest, first.out(v))
      }
    }

    loop(this, in)
  }
}

object NeuralNetwork {

  final case class SingleLayer[In <: Size, Out <: Size](layer: Layer[In, Out]) extends NeuralNetwork[In, Out]

  final case class ForwardPass[In <: Size, Mid <: Size, Out <: Size](
    first: Layer[In, Mid],
    rest:  NeuralNetwork[Mid, Out]
  ) extends NeuralNetwork[In, Out]

  def random[In <: Size, Out <: Size](
    inputs:  In,
    middle:  List[Int],
    outputs: Out,
    wRange:  (Double, Double)
  ): NeuralNetwork[In, Out] = {

    def loop[I <: Size](in:  I, mid: List[Int]): NeuralNetwork[I, Out] = mid match {
      case Nil    => SingleLayer(Layer.random(in, outputs, wRange))
      case h :: t => ForwardPass(Layer.random(in, h, wRange), loop(h, t))
    }

    loop(inputs, middle)
  }
}
