package at.doml.thesis

import java.nio.file.Paths
import at.doml.thesis.grad.{GradientDescentOptimizer, Sample}
import at.doml.thesis.nn.NeuralNetwork
import at.doml.thesis.util.Vec
import scala.collection.mutable.ListBuffer

object Main {

  private def transformToSample(ld: LabeledData): Sample[20, 20] = {
    val input = ld.points.flatMap { case (x, y) => List(x, y) }
    val target = new Array[Double](20)
    target(ld.label._1) = 1.0
    target(ld.label._2 + 10) = 1.0

    Sample(
      input  = Vec.unsafeFromIterable(input),
      target = Vec.unsafeFromIterable(target)
    )
  }

  private def displayOutput(in: List[Sample[20, 20]], nn: NeuralNetwork[20, 20]): Unit = {
    var prev = new Array[Double](10)
    val result = ListBuffer[String]()

    for (s <- in) {
      val curr = nn.out(s.input).underlying
      println(curr.mkString("(", ", ", ")"))
      val comb = curr.take(10).zip(prev).map { case (a, b) => a + b}
      var maxIndex = 0
      var max = 0.0

      for (i <- comb.indices) {
        val v = comb(i)
        if (v > max) {
          maxIndex = i
          max = v
        }
      }

      result.append(maxIndex.toString)
      prev = curr.takeRight(10).toArray
    }

    var maxIndex = 0
    var max = 0.0

    for (i <- prev.indices) {
      val v = prev(i)
      if (v > max) {
        maxIndex = i
        max = v
      }
    }

    result.append(maxIndex.toString)

    println(s"Guess: ${result.mkString}")
    println("+-------------------------------------------------------------------------------------------------------+")
  }

  def main(args: Array[String]): Unit = {
    val data = Paths.get("data/input").toFile
      .listFiles
      .filter(_.getName != "0036478777-Blue_Pen-3.png")
      .flatMap { f =>
        val data = Preprocessor.apply(f.toPath)
        println(s"[INFO] image [$f] preprocessed")
        data.map(transformToSample)
      }

    // data.length = 809

    val samples: Vec[Sample[20, 20], -1]          = Vec.unsafeFromIterable(data)
    val nn:      NeuralNetwork[20, 20]            = NeuralNetwork.random(20, List(30, 35, 40), 20, (-0.4, 0.4))
    val opt:     GradientDescentOptimizer[20, 20] = new GradientDescentOptimizer(nn)

    val train = Preprocessor.apply(Paths.get("data/input/0036478777-Blue_Pen-1.png")).map(transformToSample)
    val validate = Preprocessor.apply(Paths.get("data/input/0036478777-Blue_Pen-3.png")).map(transformToSample)

    println("+-------------------------------------------------------------------------------------------------------+")
    displayOutput(train, nn)
    displayOutput(validate, nn)
    println("+-------------------------------------------------------------------------------------------------------+")

    opt.optimize(
      samples     = samples,
      step        = 2.0,
      batchSize   = 809,
      maxIters    = 50000,
      targetError = 10e-5
    )

    println("---------------------------------------------------------------------------------------------------------")
    displayOutput(train, opt.result)
    displayOutput(validate, opt.result)
    println("---------------------------------------------------------------------------------------------------------")
  }
}
