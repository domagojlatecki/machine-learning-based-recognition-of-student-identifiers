package at.doml.thesis

import java.nio.file.Paths
import at.doml.thesis.neuralnetwork.{Layer, NeuralNetwork, Sample, SigmoidTransferFunction}

object Main {

  private val NumInputs = 40
  private val Middle = List(30)
  private val NumOutputs = 20
  private val BatchSize = 10

  private val LearningStep = 1.0
  private val MaxIterations = 10000
  private val TargetError = 10e-7

  def main(args: Array[String]): Unit = {
    println("loading image data...")
    val dataset: List[LabeledData] = Paths.get("data/input").toFile.listFiles.toList.flatMap { f =>
      Preprocessor(f.toPath)
    }
    println(s"data loaded, size: ${dataset.size}")
    val groupedDataset = dataset.map(Sample.fromLabeledData).grouped(BatchSize).toList
    val layout = NumInputs :: NumInputs :: Middle ::: List(NumOutputs)

    val layers = (for (List(in, out) <- layout.sliding(2)) yield {
      new Layer(SigmoidTransferFunction, in, out)
    }).toList
    val neuralNetwork = new NeuralNetwork(layers)
    Backpropagation(neuralNetwork, LearningStep, groupedDataset, MaxIterations, TargetError)
  }
}
