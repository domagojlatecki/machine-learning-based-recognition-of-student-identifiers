package at.doml.thesis.neuralnetwork

import at.doml.thesis.LabeledData
import scala.collection.immutable.ArraySeq

case class Sample(input: ArraySeq[Double], expectedOutput: ArraySeq[Double])

object Sample {

  def fromLabeledData(ld: LabeledData): Sample = {
    val output = new Array[Double](20)
    output(ld.label._1) = 1.0
    output(ld.label._2 + 10) = 1.0

    val input = ld.points.flatMap { case (x, y) => List(x, y) }

    Sample(input.to(ArraySeq), ArraySeq.unsafeWrapArray(output))
  }
}
