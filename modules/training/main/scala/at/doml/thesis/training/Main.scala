package at.doml.thesis.training

import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import at.doml.thesis.grad.{GradientCalc, Result, Sample}
import at.doml.thesis.nn.{Layer, NeuralNetwork, Neuron}
import at.doml.thesis.nn.NeuralNetwork.{ForwardPass, LastLayer}
import at.doml.thesis.preprocessing.{Data, Preprocessor}
import at.doml.thesis.preprocessing.image.{Canvas, Color, Point}
import at.doml.thesis.training.Command.{CreateFromLayout, LoadFromFile, PreprocessedSamples, RawSamples}
import at.doml.thesis.training.Error.!
import at.doml.thesis.util.Vec
import javax.imageio.ImageIO
import scala.annotation.tailrec
import scala.jdk.CollectionConverters.{ListHasAsScala, SeqHasAsJava}
import scala.collection.immutable.ArraySeq
import scala.collection.mutable.ArrayBuffer
import scala.util.{Failure, Success, Try}

// TODO refactor
object Main {

  private val FileNameWithLabelsRegex = "^([0-9]+)-.+?$".r

  private object PreprocessedDataExtractor {
    private val FirstPointGroup = "([^,]+,[^,;)]+)"
    private val PointGroup = "(?:;([^,]+,[^,;)]+))"
    private val DataRegex = ("^\\{label=(none|[0-9]);\\(" + FirstPointGroup + (PointGroup * 4) + "\\)}$").r

    private object PointE {
      private val PointRegex = "^([^,]+),([^,]+)$".r

      def unapply(s: String): Option[Point] = s match {
        case PointRegex(xs, ys) => xs.toDoubleOption.flatMap(x => ys.toDoubleOption.map(y => Point(x, y)))
        case _                  => None
      }
    }

    def unapply(line: String): Option[(Option[Int], Vec[Point, 5])] = {
      line match {

        case DataRegex(label, PointE(p1), PointE(p2), PointE(p3), PointE(p4), PointE(p5)) =>
          Some((label.toIntOption, Vec.unsafeWrap[Point, 5](ArraySeq(p1, p2, p3, p4, p5))))

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

  // TODO add debug data
  private def loadRawSamples(path: Path, n: Int, requireLabels: Boolean): ![Vec[Data, Int]] = {

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
          Right(Preprocessor.process(canvas, valueOf[n.type])(labels))
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
              case PreprocessedDataExtractor(label, points) =>
                label match {
                  case Some(v)                => Right(Data.Labeled(points, v.toInt))
                  case None if !requireLabels => Right(Data.Raw(points))
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

  private def loadNeuralNetwork(path: Path): ![NeuralNetwork[10, 10]] = {
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
    def loop(lines: List[String], in: Int)(acc: NeuralNetwork[in.type, 10]): ![NeuralNetwork[10, 10]] = {
      lines match {

        case Nil =>
          if (in == 10) {
            Right(acc.asInstanceOf[NeuralNetwork[10, 10]])
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
                case Left(err)      => Left(err)
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

  private def train(args: Command.Train): ![Unit] = {

    def convertToSamples(data: Vec[Data, Int]): Vec[Sample[10, 10], Int] = {
      data.map { case Data.Labeled(hotspots, label) =>
          val input = hotspots.underlying.flatMap(p => ArraySeq(p.x, p.y))
          val target = new Array[Double](10)
          target(label) = 1.0

          Sample(
            Vec.unsafeWrap(input),
            Vec.unsafeWrap(ArraySeq.unsafeWrapArray((target)))
          )
      }
    }

    def trainNetwork(nn: NeuralNetwork[10, 10], samples: Vec[Sample[10, 10], Int]): Result[10, 10] = {
      val par = new Parallel.NumProcessors()
      val result = GradientCalc.optimize(nn)(
        samples     = samples,
        step        = args.step,
        batchSize   = args.batchSize,
        maxIters    = args.maxIters,
        targetError = args.targetError
      )(par)

      par.shutdown()
      result
    }

    def writeResult(result: Result[10, 10]): ![Unit] = {
      result match {
        case Result.NoTrainingData(_) => println("No training data provided.")
        case Result.TargetError(_)    => println("Target error reached.")
        case Result.MaxIterations(_)  => println("Maximum number of iterations reached.")
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

    for {
      data    <- args.samplesPath match {
                   case RawSamples(path, numbersPerImage) => loadRawSamples(path, numbersPerImage, requireLabels = true)
                   case PreprocessedSamples(path)         => loadPreprocessedSamples(path, requireLabels = true)
                 }
      nn      <- args.neuralNetworkProvider match {
                   case LoadFromFile(path)       => loadNeuralNetwork(path)
                   case CreateFromLayout(layout) => Right(NeuralNetwork.random(10, layout, 10, (-0.4, 0.4)))
                 }
      samples  = convertToSamples(data)
      result   = trainNetwork(nn, samples)
      _       <- writeResult(result)
    } yield ()
  }

  private def test(args: Command.Test): ![Unit] = {
    final case class FlatData(hotspots: Vec[Double, 10], label: Option[Int])

    def flattenData(data: Vec[Data, Int]): Vec[FlatData, Int] = {
      data.map { d =>
        val hotspots = d.hotspots.underlying.flatMap(p => ArraySeq(p.x, p.y))
        val label = d match {
          case Data.Labeled(_, l) => Some(l)
          case Data.Raw(_)            => None
        }

        FlatData(
          Vec.unsafeWrap(hotspots),
          label
        )
      }
    }

    def printAccuracy(nn: NeuralNetwork[10, 10], data: Vec[FlatData, Int]): Unit = {
      var correct = 0
      var total = 0

      data.underlying.foreach { d =>
        val out = nn.out(d.hotspots).mapWithIndex((v, i) => (v, i)).maxBy(_._1)._2.v

        d.label match {

          case Some(label) =>
            total += 1

            if (out == label) {
              correct += 1
            }

          case None =>
            println(s"Data without label, guess: $out")
        }
      }

      println(f"Test accuracy: ${100.0 * correct / total}%.2f%%")
    }

    for {
      data     <- args.samplesPath match {
                    case RawSamples(path, n)       => loadRawSamples(path, n, requireLabels = false)
                    case PreprocessedSamples(path) => loadPreprocessedSamples(path, requireLabels = false)
                  }
      nn       <- loadNeuralNetwork(args.neuralNetworkPath)
      flatData  = flattenData(data)
    } yield printAccuracy(nn, flatData)
  }

  private def prepare(args: Command.Prepare): ![Unit] = {
    loadRawSamples(args.imagesPath, args.numbersPerImage, requireLabels = false).map { labeledData =>
      labeledData.map {

        case Data.Raw(hotspots)            =>
          hotspots.underlying.map(p => s"${p.x},${p.y}")
          .mkString("{label=none;(", ";", ")}")

        case Data.Labeled(hotspots, label) =>
          hotspots.underlying.map(p => s"${p.x},${p.y}")
          .mkString(s"{label=$label;(", ";", ")}")
      }
    }.flatMap { serializedData =>
      Try(Files.write(args.outputFile, serializedData.underlying.asJava, StandardCharsets.UTF_8)) match {
        case Failure(_) => Left(CannotWriteFileError(args.outputFile.toFile.toString))
        case _          => Right(())
      }
    }
  }

  // TODO add debug capability
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
      case cmd: Command.Train   => train(cmd)
      case cmd: Command.Test    => test(cmd)
      case cmd: Command.Prepare => prepare(cmd)
    }

    result match {
      case Left(err) => println(err)
      case _         => println("Done")
    }
  }
}
