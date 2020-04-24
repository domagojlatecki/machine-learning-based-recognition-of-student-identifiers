//package at.doml.thesis
//
//import at.doml.thesis.grad.{BatchSize, GradientCalc, Sample}
//import at.doml.thesis.nn.{Layer, NeuralNetwork}
//import at.doml.thesis.nn.NeuralNetwork.{ForwardPass, LastLayer}
//import at.doml.thesis.util.Vec
//import scala.annotation.tailrec
//
//object Main {
//
//  private def transformToSample(ld: LabeledData): Sample[10, 10] = {
//    val input = ld.points.flatMap { case (x, y) => List(x, y) }
//    val target = new Array[Double](10)
//    target(ld.label) = 1.0
//
//    Sample(
//      input  = Vec.unsafeFromIterable(input),
//      target = Vec.unsafeFromIterable(target)
//    )
//  }
//
//  private def showResult(expected: String, samples: List[Sample[10, 10]], nn: NeuralNetwork[10, 10]): Unit = {
//    val outs = samples.map { s =>
//      val y = nn.out(s.input).underlying.zipWithIndex
//      val best = y.maxBy(_._1)
//      best._2
//    }
//
//    println(s"Target: $expected, inferred: ${outs.mkString("")}")
//  }
//
//  def main(args: Array[String]): Unit = {
//    val data = Preprocessor.loadDataset().map(transformToSample)
//    val samples: Vec[Sample[10, 10], -1] = Vec.unsafeFromIterable(data)
//    val nn:      NeuralNetwork[10, 10]   = NeuralNetwork.random(10, List(15, 20), 10, (-0.4, 0.4))
//
//    val validate = Preprocessor.loadValidation().map { case (k, v) => k -> v.map(transformToSample) }
//
//    println("---------------------------------------------------------------------------------------------------------")
//    validate.foreach { case (k, v) => showResult(k, v, nn)}
//    println("---------------------------------------------------------------------------------------------------------")
//
//    val result = GradientCalc.optimize(nn)(
//      samples     = samples,
//      step        = 1.0,
//      batchSize   = BatchSize.all,
//      maxIters    = 250000,
//      targetError = 10e-5
//    ).nn
//
//    println("---------------------------------------------------------------------------------------------------------")
//    validate.foreach { case (k, v) => showResult(k, v, result)}
//    println("---------------------------------------------------------------------------------------------------------")
//
//    def printLayer[I <: Int, O <: Int](l: Layer[I, O]): Unit = {
//      println(l.neurons.underlying.map(neuron => (neuron.w.underlying, neuron.w0)))
//    }
//
//    @tailrec
//    def loop[I <: Int, O <: Int](nn: NeuralNetwork[I, O]): Unit = nn match {
//      case ForwardPass(first, rest) =>
//        printLayer(first)
//        loop(rest)
//      case LastLayer(layer) =>
//        printLayer(layer)
//    }
//
//    loop(result)
//  }
//}
