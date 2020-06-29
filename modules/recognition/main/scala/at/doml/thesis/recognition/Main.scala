package at.doml.thesis.recognition

import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import at.doml.thesis.grad.{BatchSize, GradientCalc, Result, Sample}
import at.doml.thesis.nn.{Layer, NeuralNetwork, Neuron}
import at.doml.thesis.nn.NeuralNetwork.{ForwardPass, LastLayer}
import at.doml.thesis.preprocessing.debug.CanvasDebugger
import at.doml.thesis.preprocessing.{Data, Preprocessor}
import at.doml.thesis.preprocessing.image.{Canvas, Color}
import at.doml.thesis.recognition.Command.{CreateFromLayout, LoadFromFile, PreprocessedSamples, RawSamples}
import at.doml.thesis.recognition.Error.!
import at.doml.thesis.util.collection.sized.Vec
import javax.imageio.ImageIO
import scala.annotation.tailrec
import scala.jdk.CollectionConverters.{ListHasAsScala, SeqHasAsJava}
import scala.collection.immutable.ArraySeq
import scala.collection.mutable.ArrayBuffer
import scala.util.{Failure, Success, Try}

// TODO refactor
object Main {

  private type In = Data.NumFeatures.type
  private val In: Data.NumFeatures.type = Data.NumFeatures

  private val FileNameWithLabelsRegex = "^([0-9]+)-.+?$".r

  private object PreprocessedDataExtractor {
    private val FeatureGroup = "([^,;]+)[,;]"
    private val DataRegex = ("^\\{label=(none|[0-9]);file=([^;]+);index=([0-9]+);\\(" + (FeatureGroup * In) + "\\)}$").r

    private object Features {
      def unapply(s: List[String]): Option[List[Double]] =
        Some(s.flatMap(_.toDoubleOption))
    }

    private object IntValue {
      def unapply(s: String): Option[Int] =
        s.toIntOption
    }

    def unapply(line: String): Option[(Option[Int], String, Int, Vec[Double, In])] = {
      DataRegex.unapplySeq(line) match {

        case Some(label :: file :: IntValue(index) :: Features(features)) if features.length == In =>
          Some(label.toIntOption, file, index, Vec.unsafeWrap[Double, In](features.to(ArraySeq)))

        case _ =>
          None
      }
    }
  }

  private object LayerSizeExtractor {
    private val LayerSizeRegex = "^\\{in=(\\d+);out=(\\d+);\\(.+?\\)}$".r

    def unapply(line: String): Option[(Int, Int)] = {
      line match {
        case LayerSizeRegex(in, out) => Some((in.toInt, out.toInt))
        case _                       => None
      }
    }
  }

  private object LayerNeuronsExtractor {
    private val LayerNeuronsRegex = "^\\{in=\\d+;out=\\d+;\\(\\((.+?)\\)\\)}$".r
    private val NeuronDelimiterRegex = "\\);\\(".r

    def unapply(line: String): Option[List[String]] = {
      line match {
        case LayerNeuronsRegex(neurons) => Some(NeuronDelimiterRegex.split(neurons).toList)
        case _                          => None
      }
    }
  }

  private def canvasFromImage(imagePath: File): ![Canvas] = {
    Try(ImageIO.read(imagePath)) match {

      case Failure(_) =>
        Left(CannotLoadFileError(imagePath.toString))

      case Success(image) =>
        val width = image.getWidth
        val height = image.getHeight
        val pixels = image.getRGB(0, 0, width, height, null: Array[Int], 0, width).map(Color.apply)

        Right(new Canvas(ArraySeq.unsafeWrapArray(pixels), width, height))
    }
  }

  private def loadRawSamples(
    path:          Path,
    n:             Int,
    requireLabels: Boolean,
    debugger:      CanvasDebugger
  ): ![Vec[Data, Int]] = {

    def extractLabels(f: File): Option[Vec[Int, n.type]] = {
      val extractedLabels = f.getName match {
        case FileNameWithLabelsRegex(labels) => Some(labels)
        case _                               => None
      }

      extractedLabels.filter(_.length == n)
        .map(s => s.map(_.toString.toInt))
        .map(l => Vec.unsafeWrap[Int, n.type](l.to(ArraySeq)))
    }

    def loadImage(f: File, labels: Option[Vec[Int, n.type]]): ![Vec[Data, n.type]] = {
      canvasFromImage(f).flatMap { canvas =>
        if (labels.isEmpty && requireLabels) {
          Left(InvalidFileLabelsError(f.getName))
        } else {
          Preprocessor.process(canvas, f.getName.replaceAll("\\.png$", ""), valueOf[n.type])(labels, debugger)
            .toRight(ImageProcessingError(f.getName))
        }
      }
    }

    @tailrec
    def loop(file: File, rest: List[File], acc: List[Data]): ![List[Data]] = {
      println(s"Loading file: ${file.getName}")

      val labels = extractLabels(file)
      val imageData = loadImage(file, labels).map(_.underlying.toList)

      imageData match {
        case Left(err)   => Left(err)
        case Right(data) => rest match {
          case Nil    => Right(data ::: acc)
          case f :: r => loop(f, r, data ::: acc)
        }
      }
    }

    val files = path.toFile.listFiles().toList
    val labeledData = files match {
      case Nil       => Left(NoFilesProvidedError(path.toFile.getName))
      case f :: rest => loop(f, rest, Nil)
    }

    labeledData.map(ld => Vec.unsafeWrap[Data, Int](ld.to(ArraySeq)))
  }

  private def loadPreprocessedSamples(path: Path, requireLabels: Boolean): ![Vec[Data, Int]] = {

    // TODO is order correct here?
    def loadSampleData(f: File): ![List[Data]] = {
      val res: ![List[String]] = Try(Files.readAllLines(f.toPath, StandardCharsets.UTF_8)) match {
        case Failure(_)     => Left(CannotLoadFileError(f.getName))
        case Success(lines) => Right(lines.asScala.toList)
      }

      res.flatMap { lines =>
        val init: ![List[Data]] = Right(Nil)

        lines.map(_.trim).filter(_.nonEmpty).foldLeft(init) { (acc, line) =>
          acc.flatMap { l =>
            val parsedLine = line match {
              case PreprocessedDataExtractor(label, file, index, features) =>
                label match {
                  case Some(v)                => Right(Data.Labeled(features, v.toInt, file, index))
                  case None if !requireLabels => Right(Data.Raw(features, file, index))
                  case _                      => Left(InvalidFileLabelsError(f.getName))
                }

              case l =>
                Left(ParseError(f.getName, l))
            }

            parsedLine.map(pl => pl :: l)
          }
        }
      }
    }

    val acc: ArrayBuffer[Data] = ArrayBuffer.empty
    val init: ![Unit] = Right(())

    val res = path.toFile.listFiles().foldLeft(init) { (result, f) =>
      result.flatMap(_ => loadSampleData(f).map(acc.appendAll))
    }

    res.flatMap { _ =>
      if (acc.isEmpty) {
        Left(NoDataProvidedError(path.toFile.getName))
      } else {
        Right(Vec.unsafeWrap[Data, Int](ArraySeq.unsafeWrapArray(acc.toArray)))
      }
    }
  }

  private def loadNeuralNetwork(path: Path): ![NeuralNetwork[In, 10]] = {
    val res: ![List[String]] = Try(Files.readAllLines(path, StandardCharsets.UTF_8)) match {
      case Failure(_)     => Left(CannotLoadFileError(path.toFile.getName))
      case Success(lines) => Right(lines.asScala.toList)
    }

    def parseLayerSize(line: String, out: Int): ![(Int, out.type)] = {
      line match {

        case LayerSizeExtractor(i, o) =>
          if (i < 0) {
            Left(InvalidLayerInputDimension(i))
          } else if (o != out) {
            Left(InvalidLayerOutputDimension(o))
          } else {
            Right((i, out))
          }

        case _ =>
          Left(ParseError(path.toFile.getName, line))
      }
    }

    def parseNeuron(neuron: String, in: Int): ![Neuron[in.type]] = {
      val wSplit = neuron.split(';')

      if (wSplit.length != 2) {
        Left(InvalidNeuronWeights(neuron))
      } else {
        val n = for {
          w0 <- wSplit(1).toDoubleOption
          w   = wSplit(0).split(',').flatMap(_.toDoubleOption) if w.length == in
        } yield Neuron(Vec.unsafeWrap[Double, in.type](ArraySeq.unsafeWrapArray(w)), w0)

        n.toRight(InvalidNeuronWeights(neuron))
      }
    }

    def parseNeurons(neurons: List[String], in: Int, out: Int): ![Layer[in.type, out.type]] = {
      val acc: ArrayBuffer[Neuron[in.type]] = ArrayBuffer.empty
      val init: ![Unit] = Right(())
      val err = neurons.foldLeft(init) { (err, n) =>
        err.flatMap { _ =>
          parseNeuron(n, in) match {

            case Right(neuron) =>
              acc.append(neuron)
              Right(())

            case Left(err) =>
              Left(err)
          }
        }
      }

      err.flatMap { _ =>
        val parsedNeurons = ArraySeq.unsafeWrapArray(acc.toArray)

        if (parsedNeurons.length == out) {
          Right(Layer(Vec.unsafeWrap(parsedNeurons)))
        } else {
          Left(InvalidNumberOfNeurons(parsedNeurons.length))
        }
      }
    }

    def parseLayer(line: String, in: Int, out: Int): ![Layer[in.type, out.type]] = {
      line match {
        case LayerNeuronsExtractor(neurons) => parseNeurons(neurons, in, out)
        case _                              => Left(ParseError(path.toFile.getName, line))
      }
    }

    def middleSize(in: Int, out: Int): ![in.type] = {
      if (in != out) Left(InvalidLayerOutputDimension(out)) else Right(in)
    }

    @tailrec
    def loop(lines: List[String], in: Int)(acc: NeuralNetwork[in.type, 10]): ![NeuralNetwork[In, 10]] = {
      lines match {

        case Nil =>
          if (in == In) {
            Right(acc.asInstanceOf[NeuralNetwork[In, 10]])
          } else {
            Left(InvalidLayerInputDimension(in))
          }

        case line :: rest =>
          parseLayerSize(line, in) match {

            case Left(err) => Left(err)

            case Right((inSize, outSize)) =>
              val nextAcc = for {
                mid   <- middleSize(in, outSize)
                layer <- parseLayer(line, inSize, mid)
              } yield ForwardPass(layer, acc)

              nextAcc match {
                case Left(err) => Left(err)
                case Right(nn) => loop(rest, inSize)(nn)
              }
          }

      }
    }

    res.flatMap { allLines =>
      allLines.map(_.trim).filter(_.nonEmpty) match {

        case Nil =>
          Left(EmptyFileError(path.toFile.getName))

        case line :: rest =>
          for {
            layerSize <- parseLayerSize(line, 10)
            layer     <- parseLayer(line, layerSize._1, layerSize._2)
            nn        <- loop(rest, layerSize._1)(LastLayer(layer))
          } yield nn
      }
    }
  }

  private def prepare(args: Command.Prepare): ![Unit] = {
    val debugger = args.debugRoot.map(new FileCanvasDebugger(_)).getOrElse(CanvasDebugger.NoOp)

    loadRawSamples(args.imagesPath, args.numbersPerImage, requireLabels = false, debugger).map { labeledData =>
      labeledData.map {

        case Data.Raw(features, file, index)            =>
          features.underlying.mkString(s"{label=none;file=$file;index=$index;(", ",", ";)}")

        case Data.Labeled(features, label, file, index) =>
          features.underlying.mkString(s"{label=$label;file=$file;index=$index;(", ",", ";)}")
      }
    }.flatMap { serializedData =>
      Try(Files.write(args.outputFile, serializedData.underlying.asJava, StandardCharsets.UTF_8)) match {
        case Failure(_) => Left(CannotWriteFileError(args.outputFile.toFile.toString))
        case _          => Right(())
      }
    }
  }

  private def train(args: Command.Train): ![Unit] = {

    def convertToSamples(data: Vec[Data, Int]): Vec[Sample[In, 10], Int] = {
      data.map { case Data.Labeled(features, label, _, _) =>
        val target = new Array[Double](10)
        target(label) = 1.0

        Sample(
          features,
          Vec.unsafeWrap(ArraySeq.unsafeWrapArray((target)))
        )
      }
    }

    def trainNetwork(
      nn:           NeuralNetwork[In, 10],
      trainSamples: Vec[Sample[In, 10], Int],
      testSamples:  Option[Vec[Sample[In, 10], Int]]
    ): Result[In, 10] = {
      val batchSize = args.batchSize match {
        case BatchSize.of(v) => v
        case BatchSize.all   => trainSamples.length
      }
      val par = // TODO threading for test samples
        if (batchSize <= Parallel.DefaultItemsPerThread || trainSamples.length <= Parallel.DefaultItemsPerThread) {
          Parallel.SingleThread
        } else {
          new Parallel.NumProcessors()
        }
      val result = GradientCalc.optimize(nn)(
        trainSamples          = trainSamples,
        testSamples           = testSamples,
        testMovingAverageSize = args.testMovingAverageSize,
        step                  = args.step,
        inertia               = args.inertia,
        batchSize             = args.batchSize,
        maxIters              = args.maxIters,
        targetError           = args.targetError
      )(par)

      par.shutdown()
      result
    }

    def writeResult(result: Result[In, 10]): ![Unit] = {
      result match {
        case Result.NoTrainingData(_) => println("No training data provided.")
        case Result.TargetError(_)    => println("Target error reached.")
        case Result.MaxIterations(_)  => println("Maximum number of iterations reached.")
        case Result.Fitted(_)         => println("Optimal parameters found.")
      }

      def serializeNeuron[I <: Int](neuron: Neuron[I]): String = {
        s"(${neuron.w.underlying.mkString(",")};${neuron.w0})"
      }

      def serializeLayer[I <: Int, O <: Int](layer: Layer[I, O]): String = {
        val out = layer.neurons.length
        val in = layer.neurons.underlying(0).w.length
        layer.neurons.map(serializeNeuron).underlying.mkString(s"{in=$in;out=$out;(", ";", ")}")
      }

      @tailrec
      def loop[I <: Int, O <: Int](nn: NeuralNetwork[I, O], acc: List[String]): List[String] = nn match {
        case ForwardPass(first, rest) => loop(rest, serializeLayer(first) :: acc)
        case LastLayer(layer)         => serializeLayer(layer) :: acc
      }

      val serialized = loop(result.nn, Nil)

      Try(Files.write(args.outputFile, serialized.asJava, StandardCharsets.UTF_8)) match {
        case Failure(_) => Left(CannotWriteFileError(args.outputFile.toFile.toString))
        case _          => Right(())
      }
    }

    val debugger = args.debugRoot.map(new FileCanvasDebugger(_)).getOrElse(CanvasDebugger.NoOp)
    for {
      trainData    <- args.trainSamplesPath match {

                        case RawSamples(path, numbersPerImage) =>
                          loadRawSamples(path, numbersPerImage, requireLabels = true, debugger)

                        case PreprocessedSamples(path) =>
                          loadPreprocessedSamples(path, requireLabels = true)
                      }
      testData     <- args.testSamplesPath match {

                        case Some(RawSamples(path, numbersPerImage)) =>
                          loadRawSamples(path, numbersPerImage, requireLabels = true, debugger).map(Some.apply)

                        case Some(PreprocessedSamples(path)) =>
                          loadPreprocessedSamples(path, requireLabels = true).map(Some.apply)

                        case None =>
                          Right(None)
                      }
      nn           <- args.neuralNetworkProvider match {
                        case LoadFromFile(path)       => loadNeuralNetwork(path)
                        case CreateFromLayout(layout) => Right(NeuralNetwork.random(In, layout, 10, (-0.4, 0.4)))
                      }
      trainSamples  = convertToSamples(trainData)
      testSamples   = testData.map(convertToSamples)
      result        = trainNetwork(nn, trainSamples, testSamples)
      _            <- writeResult(result)
    } yield ()
  }

  private def loadNeuralNetworks(paths: List[Path]): ![List[NeuralNetwork[In, 10]]] = {
    val init: ![List[NeuralNetwork[In, 10]]] = Right(Nil)

    paths.foldLeft(init) { (err, path) =>
      err.flatMap(loaded => loadNeuralNetwork(path).map(_ :: loaded))
    }
  }

  private final case class FlatData(features: Vec[Double, In], label: Option[Int], file: String, index: Int)

  private final case class Outputs(outs: Vec[Double, 10], label: Option[Int], file: String, index: Int)

  private final case class Prediction(prediction: Int, label: Option[Int], file: String, index: Int)

  private def flattenData(data: Vec[Data, Int]): Vec[FlatData, Int] = {
    data.map { d =>
      val label = d match {
        case Data.Labeled(_, l, _, _) => Some(l)
        case Data.Raw(_, _, _)        => None
      }

      FlatData(
        d.features,
        label,
        d.file,
        d.index
      )
    }
  }

  private def test(args: Command.Test): ![Unit] = {

    def printAccuracy(nns: List[NeuralNetwork[In, 10]], data: Vec[FlatData, Int]): Unit = {
      val allOuts = nns.map(nn => data.map(d => Outputs(nn.out(d.features), d.label, d.file, d.index)))

      val outs = if (args.ensemble) {
        val combined = allOuts.reduce { (data1, data2) =>
          data1.mapWith(data2) { (outs1, outs2) =>
            Outputs(outs1.outs.mapWith(outs2.outs)(_ + _), outs1.label, outs1.file, outs1.index)
          }
        }

        List(combined)
      } else {
        allOuts
      }

      for (out <- outs) {
        var correct = 0
        var total = 0
        val guessesByNumber = Array.fill(10, 10)(0)
        val totalByNumber = Array.fill(10)(0)
        val predictions = out.map { o =>
          Prediction(o.outs.mapWithIndex((v, i) => (v, i)).maxBy(_._1)._2.v, o.label, o.file, o.index)
        }

        predictions.underlying.foreach { p =>
          p.label match {

            case Some(label) =>
              total += 1
              totalByNumber(label) += 1
              guessesByNumber(label)(p.prediction) += 1

              if (p.prediction == label) {
                correct += 1
              }

            case None =>
              println(s"Data without label, guess: $out")
          }
        }

        val percentagesByNumber = totalByNumber.zipWithIndex.map { case (total, i) =>
          if (total == 0) {
            s"No numbers guessed as [$i]"
          } else {
            guessesByNumber(i).zipWithIndex.map { case (guess, j) =>
              f" - guessed as [$j]: ${100.0 * guess / total}%.2f%%"
            }.mkString(s"Number [$i]:\n", "\n", "\n")
          }
        }

        println(f"Test accuracy: ${100.0 * correct / total}%.2f%%")

        if (args.detailByNumber) {
          println(percentagesByNumber.mkString("\n"))
        }

        if (args.printMisses) {
          predictions.underlying.groupBy(_.file).foreach { case (file, prediction) =>
            val isMiss = prediction.exists(p => !p.label.contains(p.prediction))

            if (isMiss) {
              val sorted = prediction.sortBy(_.index).map(_.prediction).mkString("")
              println(s"$file guessed as: $sorted")
            }
          }
        }
      }
    }

    val debugger = args.debugRoot.map(new FileCanvasDebugger(_)).getOrElse(CanvasDebugger.NoOp)
    for {
      data     <- args.samplesPath match {
                    case RawSamples(path, n)       => loadRawSamples(path, n, requireLabels = false, debugger)
                    case PreprocessedSamples(path) => loadPreprocessedSamples(path, requireLabels = false)
                  }
      nns      <- loadNeuralNetworks(args.neuralNetworkPaths)
      flatData  = flattenData(data)
    } yield printAccuracy(nns, flatData)
  }

  private def apply(args: Command.Apply): ![Unit] = {

    def printRecognizedDigits(nns: List[NeuralNetwork[In, 10]], data: Vec[FlatData, Int]): Unit = {
      val allOuts = nns.map(nn => data.map(d => Outputs(nn.out(d.features), d.label, d.file, d.index)))
      val combined = allOuts.reduce { (data1, data2) =>
        data1.mapWith(data2) { (outs1, outs2) =>
          Outputs(outs1.outs.mapWith(outs2.outs)(_ + _), outs1.label, outs1.file, outs1.index)
        }
      }

      val outs = List(combined)

      for (out <- outs) {
        val predictions = out.map { o =>
          Prediction(o.outs.mapWithIndex((v, i) => (v, i)).maxBy(_._1)._2.v, o.label, o.file, o.index)
        }

        predictions.underlying.groupBy(_.file).foreach { case (file, prediction) =>
          val sorted = prediction.sortBy(_.index).map(_.prediction).mkString("")
          println(s"$file classified as: $sorted")
        }
      }
    }

    for {
      data     <- loadRawSamples(args.imagesPath, args.numbersPerImage, requireLabels = false, CanvasDebugger.NoOp)
      nns      <- loadNeuralNetworks(args.neuralNetworkPaths)
      flatData  = flattenData(data)
    } yield printRecognizedDigits(nns, flatData)
  }

  def main(args: Array[String]): Unit = {
    val command = ArgumentParser.parse(args.toList) match {
      case Right(cmd) =>
        cmd

      case Left(NoArgumentsProvidedError(help)) =>
        println(help)
        sys.exit(1)

      case Left(err) =>
        println(err)
        sys.exit(1)
    }

    val result = command match {
      case cmd: Command.Prepare => prepare(cmd)
      case cmd: Command.Train   => train(cmd)
      case cmd: Command.Test    => test(cmd)
      case cmd: Command.Apply   => apply(cmd)
    }

    result match {
      case Left(err) => println(err)
      case _         => println("Done")
    }
  }
}
