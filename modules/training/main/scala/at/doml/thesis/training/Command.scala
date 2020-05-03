package at.doml.thesis.training

import java.nio.file.Path
import at.doml.thesis.grad.BatchSize

sealed trait Command extends Product with Serializable

// TODO refactor members
object Command {

  sealed trait SamplesPath extends Product with Serializable

  final case class RawSamples(path: Path, numbersPerImage: Int) extends SamplesPath

  final case class PreprocessedSamples(path: Path) extends SamplesPath

  sealed trait NeuralNetworkProvider extends Product with Serializable

  final case class LoadFromFile(path: Path) extends NeuralNetworkProvider

  final case class CreateFromLayout(layout: List[Int]) extends NeuralNetworkProvider

  final case class Train(
    samplesPath:           SamplesPath,
    debugRoot:             Option[Path],
    outputFile:            Path,
    neuralNetworkProvider: NeuralNetworkProvider,
    step:                  Double,
    inertia:               Double,
    batchSize:             BatchSize,
    maxIters:              Int,
    targetError:           Double
  ) extends Command

  final case class Test(
    samplesPath:        SamplesPath,
    debugRoot:          Option[Path],
    neuralNetworkPaths: List[Path],
    ensemble:           Boolean,
    detailByNumber:     Boolean
  ) extends Command

  final case class Prepare(
    imagesPath:      Path,
    debugRoot:       Option[Path],
    numbersPerImage: Int,
    outputFile:      Path
  ) extends Command

  final case class Analyze(
    hotspotsPath: Path
  ) extends Command
}
