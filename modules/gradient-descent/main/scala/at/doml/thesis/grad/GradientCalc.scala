package at.doml.thesis.grad

import at.doml.thesis.grad.Result.NoTrainingData
import at.doml.thesis.grad.internal.{AccGrads, LayerData, LayerGrads, NeuralNetworkData, NeuronGrads}
import at.doml.thesis.grad.internal.AccGrads.{ForwardPass, LastLayer}
import at.doml.thesis.grad.internal.NeuralNetworkData.{BackwardPassData, FirstLayerData}
import at.doml.thesis.nn.{Layer, NeuralNetwork, Neuron}
import at.doml.thesis.util.{Parallel, Vec}
import scala.annotation.tailrec
import scala.collection.immutable.ArraySeq

// TODO refactor
object GradientCalc {

  private def calcLayerOutputs[In <: Int, Out <: Int, N <: Int](
    sampleInputs: Vec[Vec[Double, In], N],
    nn:           AccGrads[In, Out]
  )(implicit par: Parallel): NeuralNetworkData[In, Out, N] = {

     @tailrec
     def loop[FI <: Int, I <: Int](
       n:   AccGrads[I, Out],
       ins: Vec[Vec[Double, I], N],
       acc: NeuralNetworkData[FI, I, N]
     ): NeuralNetworkData[FI, Out, N] =
       n match {

         case ForwardPass(first, rest) =>
           val outs = ins.parMap(first.out)
           loop(rest, outs, BackwardPassData(LayerData(first.layer.neurons, ins, outs, first.accGrads), acc))

         case LastLayer(lg) =>
           BackwardPassData(LayerData(lg.layer.neurons, ins, ins.parMap(lg.layer.out), lg.accGrads), acc)
       }


     nn match {

       case ForwardPass(first, rest) =>
         val outs = sampleInputs.parMap(first.out)
         val layerData = LayerData(first.layer.neurons, sampleInputs, outs, first.accGrads)
         loop(rest, outs, FirstLayerData(layerData))

       case LastLayer(lg) =>
         FirstLayerData(LayerData(lg.layer.neurons, sampleInputs, sampleInputs.parMap(lg.layer.out), lg.accGrads))
     }
   }

  private def calcNextWeights[In <: Int, Out <: Int, N <: Int](
    sampleTargets:    Vec[Vec[Double, Out], N],
    nnd:              NeuralNetworkData[In, Out, N],
    step:             Double,
    gradCoeff:        Double,
    gradDecay:        Double
  )(implicit par: Parallel): AccGrads[In, Out] = {

    def calcLastLayerDeltas[I <: Int](layerData: LayerData[I, Out, N]): Vec[Vec[Double, Out], N] = {
      layerData.sampleOutputs.parMapWith(sampleTargets) { (outputs, targets) =>
        outputs.mapWith(targets)((y, t) => y * (1.0 - y) * (t - y))
      }
    }

    def calcLayerDeltas[I <: Int, O <: Int, NO <: Int](
      layerData:   LayerData[I, O, N],
      prevNeurons: Vec[Neuron[O], NO],
      prevDeltas:  Vec[Vec[Double, NO], N]
    ): Vec[Vec[Double, O], N] = {
      layerData.sampleOutputs.parMapWithIndex { (outputs, n) =>
        outputs.mapWithIndex { (y, o) =>
          var sum = 0.0

          for (no <- prevNeurons.indices) {
            sum += prevDeltas(n)(no) * prevNeurons(no).w(o)
          }

          y * (1.0 - y) * sum
        }
      }
    }

    def calcGradients[I <: Int, O <: Int](
      layerData: LayerData[I, O, N],
      deltas:    Vec[Vec[Double, O], N]
    ): Vec[NeuronGrads[I], O] = {
      layerData.neurons.mapWithIndex { (neuron, o) =>
        val w = neuron.w.mapWithIndex { (_, i) =>
          val tasks = layerData.sampleInputs.indices.grouped(par.itemsPerThread).map { indices =>
            () => {
              var sum = 0.0

              for (n <- indices) {
                val y = layerData.sampleInputs(n)(i)
                val d = deltas(n)(o)
                sum += d * y
              }

              sum
            }
          }

          par.execute(tasks).sum
        }

        val w0Tasks = layerData.sampleInputs.indices.grouped(par.itemsPerThread).map { indices =>
          () => {
            var sum = 0.0

            for (n <- indices) {
              sum += deltas(n)(o)
            }

            sum
          }
        }

        val w0 = par.execute(w0Tasks).sum

        NeuronGrads(w, w0)
      }
    }

    def applyGradients[I <: Int, O <: Int, NO <: Int](
      layerData: LayerData[I, O, N],
      grads:     Vec[NeuronGrads[I], O]
    ): LayerGrads[I, O] = {
      // TODO use all grad history
      LayerGrads(
        Layer(
          layerData.neurons.mapWith(grads, layerData.accGrads) { (neuron, grad, accGrads) =>
            Neuron(
              neuron.w.mapWith(grad.w, accGrads.w)((w, dw, acc) => w + gradCoeff * step * dw + gradDecay * step * acc),
              neuron.w0 + gradCoeff * step * grad.w0 + gradDecay * step * accGrads.w0
            )
          }
        ),
        grads
      )
    }

    @tailrec
    def loop[I <: Int, O <: Int, NO <: Int, FO <: Int](
      n:           NeuralNetworkData[In, O, N],
      prevNeurons: Vec[Neuron[O], NO],
      prevDeltas:  Vec[Vec[Double, NO], N],
      acc:         AccGrads[O, FO]
    ): AccGrads[In, FO] = n match {

      case BackwardPassData(layerData, previous) =>
        val deltas = calcLayerDeltas(layerData, prevNeurons, prevDeltas)
        val grads = calcGradients(layerData, deltas)
        val layer = applyGradients(layerData, grads)
        loop(previous, layerData.neurons, deltas, ForwardPass(layer, acc))

      case FirstLayerData(layerData) =>
        val deltas = calcLayerDeltas(layerData, prevNeurons, prevDeltas)
        val grads = calcGradients(layerData, deltas)
        val layer = applyGradients(layerData, grads)
        ForwardPass(layer, acc)
    }

    nnd match {

      case BackwardPassData(layerData, previous) =>
        val deltas = calcLastLayerDeltas(layerData)
        val grads = calcGradients(layerData, deltas)
        val layer = applyGradients(layerData, grads)
        loop(previous, layerData.neurons, deltas, LastLayer(layer))

      case FirstLayerData(layerData) =>
        val deltas = calcLastLayerDeltas(layerData)
        val grads = calcGradients(layerData, deltas)
        val layer = applyGradients(layerData, grads)
        LastLayer(layer)
    }
  }

  private def calcError[In <: Int, Out <: Int, N <: Int](
    targets: Vec[Vec[Double, Out], N],
    inputs:  Vec[Vec[Double, In], N],
    nn:      AccGrads[In, Out]
  )(implicit par: Parallel): Double = {
    val outputs = inputs.parMap(nn.out)
    val tasks = targets.indices.grouped(par.itemsPerThread).map { indices =>
      () => {
        var sum = 0.0

        for (n <- indices) {
          val target = targets(n)
          val output = outputs(n)

          for (o <- target.indices) {
            sum += math.pow(target(o) - output(o), 2.0)
          }
        }

        sum
      }
    }

    par.execute(tasks).sum / (2.0 * targets.length)
  }

  private def errorColor(previous: Double, next: Double): String = {
    if (previous > next) {
      "\u001B[32m"
    } else if (previous < next) {
      "\u001B[31m"
    } else {
      "\u001B[33m"
    }
  }

  private sealed trait TestValidation[In <: Int, Out <: Int] extends Product with Serializable {
    def updateCurrentError(acc: AccGrads[In, Out])(implicit par: Parallel): TestValidation[In, Out]
    def testErrorString: String
  }

  private final case class TestErrorData[In <: Int, Out <: Int, N <: Int](
    currentError:          Double,
    bestError:             Double,
    bestAcc:               AccGrads[In, Out],
    history:               List[Double],
    previousMovingAverage: Double,
    movingAverageSize:     Int,
    testInputs:            Vec[Vec[Double, In], N],
    testTargets:           Vec[Vec[Double, Out], N]
  ) extends TestValidation[In, Out] {

    def updateCurrentError(acc: AccGrads[In, Out])(implicit par: Parallel): TestValidation[In, Out] = {
      val error = calcError(testTargets, testInputs, acc)
      val hSize = history.size
      val prevAvg = history match {
        case Nil => error
        case h   => h.sum / hSize
      }
      val newHistory = if (hSize >= movingAverageSize) {
        error :: history.init
      } else {
        error :: history
      }

      copy(
        currentError          = error,
        bestError             = error min bestError,
        bestAcc               = if (error <= bestError) acc else bestAcc,
        history               = newHistory,
        previousMovingAverage = prevAvg
      )
    }

    def testErrorString: String = {
      history match {
        case Nil                   => ""
        case err :: Nil            => s"; test error: $err"
        case next :: previous :: _ =>
          val movingAverageString = if (movingAverageSize > 1) {
            val nextAvg = history.sum / history.size
            s"; ${errorColor(previousMovingAverage, nextAvg)}moving average test error: $nextAvg\u001B[0m"
          } else {
            ""
          }

          s"$movingAverageString; ${errorColor(previous, next)}test error: $next\u001B[0m"
      }
    }
  }

  private final case class NoTestData[In <: Int, Out <: Int]() extends TestValidation[In, Out] {
    def updateCurrentError(acc: AccGrads[In, Out])(implicit par: Parallel): TestValidation[In, Out] = this
    def testErrorString: String = ""
  }

  def optimize[In <: Int, Out <: Int, N1 <: Int, N2 <: Int](
    nn: NeuralNetwork[In, Out]
  )(
    trainSamples:                 Vec[Sample[In, Out], N1],
    testSamples:                  Option[Vec[Sample[In, Out], N2]],
    testSamplesMovingAverageSize: Option[Int],
    step:                         Double,
    inertia:                      Double,
    batchSize:                    BatchSize,
    maxIters:                     Int,
    targetError:                  Double
  )(implicit par: Parallel): Result[In, Out] = {
    val trainInputs = trainSamples.map(_.input)
    val trainTargets = trainSamples.map(_.target)

    val numOfBatchSamples = batchSize match {
      case BatchSize.of(v) => v
      case BatchSize.all   => trainSamples.length
    }

    val inputBatches = trainInputs.underlying.grouped(numOfBatchSamples).toList
    val targetBatches = trainTargets.underlying.grouped(numOfBatchSamples).toList

    type Batch = (ArraySeq[Vec[Double, In]], ArraySeq[Vec[Double, Out]])
    val batches: List[Batch] = inputBatches.zip(targetBatches)

    @tailrec
    def batchLoop(n: AccGrads[In, Out], batch: Batch, rest: List[Batch]): AccGrads[In, Out] = {
      val len = batch._1.length
      val input: Vec[Vec[Double, In], len.type] = Vec.unsafeWrap(batch._1)
      val target: Vec[Vec[Double, Out], len.type] = Vec.unsafeWrap(batch._2)
      val adjustedStep = step / len
      val nnd = calcLayerOutputs(input, n)
      val next = calcNextWeights(target, nnd, adjustedStep, 1.0 - inertia, inertia)

      rest match {
        case Nil    => next
        case h :: t => batchLoop(next, h, t)
      }
    }

    @tailrec
    def loop(
      n:              AccGrads[In, Out],
      iter:           Int,
      trainErr:       Double,
      testValidation: TestValidation[In, Out]
    ): Result[In, Out] = {
      if (trainErr <= targetError) {
        Result.TargetError(AccGrads.unwrap(n))
      } else if (iter > maxIters) {
        Result.MaxIterations(AccGrads.unwrap(n))
      } else {
        batches match {
          case Nil    => NoTrainingData(AccGrads.unwrap(n))
          case h :: t =>
            val next = batchLoop(n, h, t)
            val trainError = calcError(trainTargets, trainInputs, next)
            val nextTestValidation = testValidation.updateCurrentError(next)

            println(
              s"${errorColor(trainErr, trainError)}Iteration $iter error: $trainError\u001B[0m" +
              s"${nextTestValidation.testErrorString}"
            )

            nextTestValidation match {

              case NoTestData() =>
                loop(next, iter + 1, trainError, nextTestValidation)

              case TestErrorData(_, _, bestAcc, history, previousMovingAverage, _, _, _) =>
                val nextMovingAverage = history.sum / history.size

                if (nextMovingAverage > previousMovingAverage) {
                  Result.Fitted(AccGrads.unwrap(bestAcc))
                } else {
                  loop(next, iter + 1, trainError, nextTestValidation)
                }
            }
        }
      }
    }

    val acc = AccGrads.wrap(nn) // TODO initialize to iteration 1 gradients
    val trainError = calcError(trainTargets, trainInputs, acc)
    val testValidation: TestValidation[In, Out] = testSamples match {
      case None    => NoTestData()
      case Some(s) =>
        TestErrorData(
          currentError          = 0.0,
          bestError             = Double.PositiveInfinity,
          bestAcc               = acc,
          history               = Nil,
          previousMovingAverage = 0.0,
          movingAverageSize     = testSamplesMovingAverageSize.getOrElse(1),
          testInputs            = s.map(_.input),
          testTargets           = s.map(_.target)
        ).updateCurrentError(acc)
    }

    println(s"Initial error: $trainError${testValidation.testErrorString}")

    loop(acc, 1, trainError, testValidation)
  }
}
