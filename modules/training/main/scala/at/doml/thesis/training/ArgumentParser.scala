package at.doml.thesis.training

import java.nio.file.{Path, Paths}
import at.doml.thesis.grad.BatchSize
import at.doml.thesis.training.Command._
import at.doml.thesis.training.Error.!
import scala.annotation.tailrec

// TODO refactor this
object ArgumentParser {

  private trait NamedState {
    val name: String
    val desc: String
    val `type`: String
  }

  private trait CommandParser {

    type S
    type NS <: S with NamedState
    type Acc

    val commandName: String
    val namedStates: List[NS]
    val initState: S
    val endStates: Set[S]
    val emptyAcc: Acc

    def applyState(arg: String, state: S, acc: Acc): ![(S, Acc)]

    def buildArgs(acc: Acc): ![Command]

    def parseArgs(arguments: List[String]): ![Command] = {

      @tailrec
      def loop(args: List[String], state: S, acc: Acc): ![Command] = args match {

        case Nil =>
          if (endStates.contains(state)) {
            buildArgs(acc)
          } else {
            Left(MissingArgumentError(state.toString))
          }

        case arg :: rest =>
          applyState(arg, state, acc) match {
            case Right((nextState, nextAcc)) => loop(rest, nextState, nextAcc)
            case Left(err)                   => Left(err)
          }
      }

      loop(arguments, initState, emptyAcc)
    }

    final def helpMessage: String = {
      val maxArgNameLength = namedStates.map(_.name.length).max
      val maxTypeLength = namedStates.map(_.`type`.length).max

      val stateHelpTexts = namedStates.map { s =>
        val argNameDiff = maxArgNameLength - s.name.length
        val typeDiff = maxTypeLength - s.`type`.length

        s"    ${s.name}${" " * argNameDiff} [${s.`type`}]${" " * typeDiff} ${s.desc}"
      }

      s"""Available arguments for '$commandName' command:
         |
         |${stateHelpTexts.mkString("\n")}
         |""".stripMargin
    }
  }

  private object TrainCommandParser extends CommandParser {

    sealed trait State extends Product with Serializable

    sealed abstract class Arg(val name: String, val desc: String, val `type`: String) extends State with NamedState {
      override def toString: String = name
    }

    object State {
      case object Init            extends State
      case object Step            extends Arg("--step", "Learning step, default value: 1.0", "double")
      case object Inertia         extends Arg("--inertia", "Gradient inertia, default value: 0.0", "double")
      case object BatchSize       extends Arg("--batch-size", "Batch size, default value: 'all'", "double | 'all'")
      case object MaxIters        extends Arg("--max-iters", "Max number of iterations, default value: 10000", "int")
      case object TargetError     extends Arg("--target-error", "Target error, default value: 10e-7", "double")
      case object RawSamples      extends Arg("--images", "Root of image dataset, exclusive with --features", "dir")
      case object PrepSamples     extends Arg("--features", "Root of features dataset, exclusive with --images", "dir")
      case object NumbersPerImage extends Arg("--n-per-image", "Amount of numbers in each image, default: 10", "int")
      case object OutputPath      extends Arg("--output", "Output file for neural network", "file")
      case object LoadNetwork     extends Arg("--load", "Load neural network file, exclusive with --layout", "file")
      case object DebugRoot       extends Arg("--debug", "Image debug data root, optional", "dir")
      case object Layout          extends Arg(
        "--layout",
        "Middle layers for neural network, exclusive with --load, default: 10x10",
        "int['x'int...]"
      )
    }

    final case class ArgsBuilder(
      samplesPath:           Option[SamplesPath]   = None,
      debugRoot:             Option[Path]          = None,
      outputFile:            Option[Path]          = None,
      neuralNetworkProvider: NeuralNetworkProvider = CreateFromLayout(List(10 ,10)),
      numbersPerImage:       Int                   = 10,
      step:                  Double                = 1.0,
      inertia:               Double                = 0.0,
      batchSize:             BatchSize             = BatchSize.all,
      maxIters:              Int                   = 10_000,
      targetError:           Double                = 10e-7
    )

    type S = State
    type NS = Arg
    type Acc = ArgsBuilder

    val commandName: String = "train"

    val namedStates: List[Arg] = List(
      State.Step,
      State.Inertia,
      State.BatchSize,
      State.MaxIters,
      State.TargetError,
      State.RawSamples,
      State.PrepSamples,
      State.NumbersPerImage,
      State.OutputPath,
      State.LoadNetwork,
      State.DebugRoot,
      State.Layout
    )

    val initState: State = State.Init

    val endStates: Set[State] = Set(initState)

    val emptyAcc: ArgsBuilder = ArgsBuilder()

    def applyState(arg: String, state: State, acc: ArgsBuilder): ![(State, ArgsBuilder)] = {
      state match {

        case State.Init =>
          namedStates.find(_.name == arg) match {
            case Some(nextState) => Right((nextState, acc))
            case None            => Left(UnknownArgumentError(arg))
          }

        case State.Step =>
          arg.toDoubleOption match {
            case Some(v) => Right((State.Init, acc.copy(step = v)))
            case None    => Left(NumberParseError(arg))
          }

        case State.Inertia =>
          arg.toDoubleOption match {
            case Some(v) if v >= 0.0 && v <= 1.0 => Right((State.Init, acc.copy(inertia = v)))
            case Some(_)                         => Left(IllegalArgumentError(arg))
            case None                            => Left(NumberParseError(arg))
          }

        case State.BatchSize =>
          arg match {
            case "all" => Right((State.Init, acc.copy(batchSize = BatchSize.all)))
            case _     => arg.toIntOption match {
              case Some(v) if v > 0 => Right((State.Init, acc.copy(batchSize = BatchSize.of(v))))
              case Some(_)          => Left(IllegalArgumentError(arg))
              case None             => Left(NumberParseError(arg))
            }
          }

        case State.MaxIters =>
          arg.toIntOption match {
            case Some(v) if v >= 0 => Right((State.Init, acc.copy(maxIters = v)))
            case Some(_)           => Left(IllegalArgumentError(arg))
            case None              => Left(NumberParseError(arg))
          }

        case State.TargetError =>
          arg.toDoubleOption match {
            case Some(v) if v >= 0.0 => Right((State.Init, acc.copy(targetError = v)))
            case Some(_)             => Left(IllegalArgumentError(arg))
            case None                => Left(NumberParseError(arg))
          }

        case State.RawSamples =>
          val path = Paths.get(arg)

          if (path.toFile.isDirectory) {
            Right((State.Init, acc.copy(samplesPath = Some(RawSamples(path, 0)))))
          } else {
            Left(IllegalArgumentError(arg))
          }

        case State.PrepSamples =>
          val path = Paths.get(arg)

          if (path.toFile.isDirectory) {
            Right((State.Init, acc.copy(samplesPath = Some(PreprocessedSamples(path)))))
          } else {
            Left(IllegalArgumentError(arg))
          }

        case State.NumbersPerImage =>
          arg.toIntOption match {
            case Some(v) if v > 0 => Right((State.Init, acc.copy(numbersPerImage = v)))
            case Some(_)          => Left(IllegalArgumentError(arg))
            case None             => Left(NumberParseError(arg))
          }

        case State.OutputPath =>
          val path = Paths.get(arg)

          if (!path.toFile.isDirectory) {
            Right((State.Init, acc.copy(outputFile = Some(path))))
          } else {
            Left(IllegalArgumentError(arg))
          }

        case State.LoadNetwork =>
          val path = Paths.get(arg)

          if (path.toFile.isFile) {
            Right((State.Init, acc.copy(neuralNetworkProvider = LoadFromFile(path))))
          } else {
            Left(IllegalArgumentError(arg))
          }

        case State.DebugRoot =>
          val path = Paths.get(arg)

          if (path.toFile.isFile) {
            Left(IllegalArgumentError(arg))
          } else {
            Right((State.Init, acc.copy(debugRoot = Some(path))))
          }

        case State.Layout =>
          val init: Option[List[Int]] = Some(Nil)
          val result = arg.split('x').map(_.toIntOption).foldLeft(init) { (list, int) =>
            list.flatMap(l => int.map(i => i :: l))
          }

          result match {

            case Some(v) if v.forall(_ > 0) =>
              Right((State.Init, acc.copy(neuralNetworkProvider = CreateFromLayout(v))))

            case _ =>
              Left(IllegalArgumentError(arg))
          }
      }
    }

    def buildArgs(acc: ArgsBuilder): ![Command.Train] = {

      (acc.samplesPath, acc.outputFile) match {
        case (Some(samplesPath), Some(outputFile)) =>
          val sp = samplesPath match {
            case RawSamples(p, _) => RawSamples(p, acc.numbersPerImage)
            case v                => v
          }

          Right(
            Command.Train(
              samplesPath = sp,
              debugRoot = acc.debugRoot,
              outputFile = outputFile,
              neuralNetworkProvider = acc.neuralNetworkProvider,
              step = acc.step,
              inertia = acc.inertia,
              batchSize = acc.batchSize,
              maxIters = acc.maxIters,
              targetError = acc.targetError
            )
          )

        case (Some(_), None) =>
          Left(MissingArgumentError("--output"))

        case (None, Some(_)) =>
          Left(MissingArgumentError("--images or --features"))

        case (None, None) =>
          Left(MissingArgumentError("--images or --features, --output"))
      }
    }
  }

  private object TestCommandParser extends CommandParser {

    sealed trait State extends Product with Serializable

    sealed abstract class Arg(val name: String, val desc: String, val `type`: String) extends State with NamedState {
      override def toString: String = name
    }

    object State {
      case object Init            extends State
      case object RawSamples      extends Arg("--images", "Root of image dataset, exclusive with --features", "dir")
      case object PrepSamples     extends Arg("--features", "Root of features dataset, exclusive with --images", "dir")
      case object NumbersPerImage extends Arg("--n-per-image", "Amount of numbers in each image, default: 10", "int")
      case object DebugRoot       extends Arg("--debug", "Image debug data root, optional", "dir")
      case object NetworkPaths    extends Arg(
        "--nn-paths",
        "Path to neural network files, must be the last argument",
        "file [file...]"
      )
      case object Ensemble        extends Arg(
        "--ensemble",
        "Ensemble neural networks into a single classifier, disabled by default",
        ""
      )
      case object DetailByNumber  extends Arg(
        "--detail-by-number",
        "Print detailed statistics by number, disabled by default",
        ""
      )
    }

    final case class ArgsBuilder(
      samplesPath:        Option[SamplesPath] = None,
      debugRoot:          Option[Path]        = None,
      neuralNetworkPaths: List[Path]          = Nil,
      numbersPerImage:    Int                 = 10,
      ensemble:           Boolean             = false,
      detailByNumber:     Boolean             = false
    )

    type S = State
    type NS = Arg
    type Acc = ArgsBuilder

    val commandName: String = "test"

    val namedStates: List[Arg] = List(
      State.RawSamples,
      State.PrepSamples,
      State.NumbersPerImage,
      State.Ensemble,
      State.DebugRoot,
      State.NetworkPaths,
      State.DetailByNumber
    )

    val initState: State = State.Init

    val endStates: Set[State] = Set(initState, State.NetworkPaths)

    val emptyAcc: ArgsBuilder = ArgsBuilder()

    def applyState(arg: String, state: State, acc: ArgsBuilder): ![(State, ArgsBuilder)] = {
      state match {

        case State.Init =>
          namedStates.find(_.name == arg) match {
            case Some(State.Ensemble)       => Right((State.Init, acc.copy(ensemble = true)))
            case Some(State.DetailByNumber) => Right((State.Init, acc.copy(detailByNumber = true)))
            case Some(nextState)            => Right((nextState, acc))
            case None                       => Left(UnknownArgumentError(arg))
          }

        case State.RawSamples =>
          val path = Paths.get(arg)

          if (path.toFile.isDirectory) {
            Right((State.Init, acc.copy(samplesPath = Some(RawSamples(path, 0)))))
          } else {
            Left(IllegalArgumentError(arg))
          }

        case State.PrepSamples =>
          val path = Paths.get(arg)

          if (path.toFile.isDirectory) {
            Right((State.Init, acc.copy(samplesPath = Some(PreprocessedSamples(path)))))
          } else {
            Left(IllegalArgumentError(arg))
          }

        case State.NumbersPerImage =>
          arg.toIntOption match {
            case Some(v) if v > 0 => Right((State.Init, acc.copy(numbersPerImage = v)))
            case Some(_)          => Left(IllegalArgumentError(arg))
            case None             => Left(NumberParseError(arg))
          }

        case State.DebugRoot =>
          val path = Paths.get(arg)

          if (path.toFile.isFile) {
            Left(IllegalArgumentError(arg))
          } else {
            Right((State.Init, acc.copy(debugRoot = Some(path))))
          }

        case State.NetworkPaths =>
          val path = Paths.get(arg)

          if (path.toFile.isFile) {
            Right((State.NetworkPaths, acc.copy(neuralNetworkPaths = path :: acc.neuralNetworkPaths)))
          } else {
            Left(IllegalArgumentError(arg))
          }
      }
    }

    def buildArgs(acc: ArgsBuilder): ![Command.Test] = {

      (acc.samplesPath, acc.neuralNetworkPaths) match {
        case (Some(samplesPath), first :: rest) =>
          val sp = samplesPath match {
            case RawSamples(p, _) => RawSamples(p, acc.numbersPerImage)
            case v                => v
          }

          Right(
            Command.Test(
              samplesPath = sp,
              debugRoot = acc.debugRoot,
              neuralNetworkPaths = first :: rest,
              ensemble = acc.ensemble,
              detailByNumber = acc.detailByNumber
            )
          )

        case (Some(_), Nil) =>
          Left(MissingArgumentError("--nn-paths"))

        case (None, _ :: _) =>
          Left(MissingArgumentError("--images or --features"))

        case (None, Nil) =>
          Left(MissingArgumentError("--images or --features, --nn-paths"))
      }
    }
  }

  private object PrepareCommandParser extends CommandParser {

    sealed trait State extends Product with Serializable

    sealed abstract class Arg(val name: String, val desc: String, val `type`: String) extends State with NamedState {
      override def toString: String = name
    }

    object State {
      case object Init            extends State
      case object InputPath       extends Arg("--images", "Root of image dataset", "dir")
      case object NumbersPerImage extends Arg("--n-per-image", "Amount of numbers in each image, default: 10", "int")
      case object OutputPath      extends Arg("--output", "Processed images output file", "file")
      case object DebugRoot       extends Arg("--debug", "Image debug data root, optional", "dir")
    }

    final case class ArgsBuilder(
      imagesPath:      Option[Path] = None,
      debugRoot:       Option[Path] = None,
      outputFile:      Option[Path] = None,
      numbersPerImage: Int          = 10
    )

    type S = State
    type NS = Arg
    type Acc = ArgsBuilder

    val commandName: String = "prepare"

    val namedStates: List[Arg] = List(
      State.InputPath,
      State.NumbersPerImage,
      State.OutputPath,
      State.DebugRoot
    )

    val initState: State = State.Init

    val endStates: Set[State] = Set(initState)

    val emptyAcc: ArgsBuilder = ArgsBuilder()

    def applyState(arg: String, state: State, acc: ArgsBuilder): ![(State, ArgsBuilder)] = {
      state match {

        case State.Init =>
          namedStates.find(_.name == arg) match {
            case Some(nextState) => Right((nextState, acc))
            case None            => Left(UnknownArgumentError(arg))
          }

        case State.InputPath =>
          val path = Paths.get(arg)

          if (path.toFile.isDirectory) {
            Right((State.Init, acc.copy(imagesPath = Some(path))))
          } else {
            Left(IllegalArgumentError(arg))
          }

        case State.NumbersPerImage =>
          arg.toIntOption match {
            case Some(v) if v > 0 => Right((State.Init, acc.copy(numbersPerImage = v)))
            case Some(_)          => Left(IllegalArgumentError(arg))
            case None             => Left(NumberParseError(arg))
          }

        case State.OutputPath =>
          val path = Paths.get(arg)

          if (!path.toFile.isDirectory) {
            Right((State.Init, acc.copy(outputFile = Some(path))))
          } else {
            Left(IllegalArgumentError(arg))
          }

        case State.DebugRoot =>
          val path = Paths.get(arg)

          if (path.toFile.isFile) {
            Left(IllegalArgumentError(arg))
          } else {
            Right((State.Init, acc.copy(debugRoot = Some(path))))
          }
      }
    }

    def buildArgs(acc: ArgsBuilder): ![Command.Prepare] = {

      (acc.imagesPath, acc.outputFile) match {
        case (Some(imagesPath), Some(outputFile)) =>
          Right(
            Command.Prepare(
              imagesPath = imagesPath,
              debugRoot = acc.debugRoot,
              numbersPerImage = acc.numbersPerImage,
              outputFile = outputFile
            )
          )

        case (Some(_), None) =>
          Left(MissingArgumentError("--output"))

        case (None, Some(_)) =>
          Left(MissingArgumentError("--images"))

        case (None, None) =>
          Left(MissingArgumentError("--images, --output"))
      }
    }
  }

  private object AnalyzeCommandParser extends CommandParser {

    sealed trait State extends Product with Serializable

    sealed abstract class Arg(val name: String, val desc: String, val `type`: String) extends State with NamedState {
      override def toString: String = name
    }

    object State {
      case object Init     extends State
      case object Features extends Arg("--features", "Root of features dataset", "dir")
    }

    final case class ArgsBuilder(
      featuresPath: Option[Path] = None
    )

    type S = State
    type NS = Arg
    type Acc = ArgsBuilder

    val commandName: String = "analyze"

    val namedStates: List[Arg] = List(
      State.Features
    )

    val initState: State = State.Init

    val endStates: Set[State] = Set(initState)

    val emptyAcc: ArgsBuilder = ArgsBuilder()

    def applyState(arg: String, state: State, acc: ArgsBuilder): ![(State, ArgsBuilder)] = {
      state match {

        case State.Init =>
          namedStates.find(_.name == arg) match {
            case Some(nextState) => Right((nextState, acc))
            case None            => Left(UnknownArgumentError(arg))
          }

        case State.Features =>
          val path = Paths.get(arg)

          if (path.toFile.isDirectory) {
            Right((State.Init, acc.copy(featuresPath = Some(path))))
          } else {
            Left(IllegalArgumentError(arg))
          }
      }
    }

    def buildArgs(acc: ArgsBuilder): ![Command.Analyze] = {

      acc.featuresPath match {
        case Some(featuresPath) =>
          Right(
            Command.Analyze(
              featuresPath = featuresPath
            )
          )

        case None =>
          Left(MissingArgumentError("--features"))
      }
    }
  }

  def parse(arguments: List[String]): ![Command] = {
    def helpMessage: String =
      s"""Usage: command [arguments...]
         |Available commands: train, test, prepare, analyze
         |
         |${TrainCommandParser.helpMessage}
         |${TestCommandParser.helpMessage}
         |${PrepareCommandParser.helpMessage}
         |${AnalyzeCommandParser.helpMessage}
         |""".stripMargin

    arguments match {

      case Nil =>
        Left(NoArgumentsProvidedError(helpMessage))

      case command :: args =>
        command match {
          case "train"   => TrainCommandParser.parseArgs(args)
          case "test"    => TestCommandParser.parseArgs(args)
          case "prepare" => PrepareCommandParser.parseArgs(args)
          case "analyze" => AnalyzeCommandParser.parseArgs(args)
          case _         => Left(UnknownCommandError(command))
        }
    }
  }
}
