package at.doml.thesis.grad

import at.doml.thesis.grad.Result.NoTrainingData
import at.doml.thesis.grad.internal.AccGrads
import at.doml.thesis.nn.NeuralNetwork
import at.doml.thesis.util.collection.sized.Vec
import at.doml.thesis.util.par.Parallel
import scala.annotation.tailrec
import scala.collection.immutable.ArraySeq

object GradientCalc {

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
      val error = calc.Error(testTargets, testInputs, acc)
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
        case Nil        => ""
        case err :: Nil => s"; test error: $err"
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

  // format: off
  def optimize[In <: Int, Out <: Int, N1 <: Int, N2 <: Int](
    nn: NeuralNetwork[In, Out]
  )(
    trainSamples:          Vec[Sample[In, Out], N1],
    testSamples:           Option[Vec[Sample[In, Out], N2]],
    testMovingAverageSize: Option[Int],
    step:                  Double,
    inertia:               Double,
    batchSize:             BatchSize,
    maxIters:              Int,
    targetError:           Double,
    printStatus:           String => Unit = println
  )(implicit par: Parallel): Result[In, Out] = {
    // format: on
    val trainInputs  = trainSamples.map(_.input)
    val trainTargets = trainSamples.map(_.target)

    val numOfBatchSamples = batchSize match {
      case BatchSize.of(v) => v
      case BatchSize.all   => trainSamples.length
    }

    val inputBatches  = trainInputs.underlying.grouped(numOfBatchSamples).toList
    val targetBatches = trainTargets.underlying.grouped(numOfBatchSamples).toList

    type Batch = (ArraySeq[Vec[Double, In]], ArraySeq[Vec[Double, Out]])
    val batches: List[Batch] = inputBatches.zip(targetBatches)

    @tailrec
    def batchLoop(n: AccGrads[In, Out], batch: Batch, rest: List[Batch]): AccGrads[In, Out] = {
      val len                                     = batch._1.length
      val input: Vec[Vec[Double, In], len.type]   = Vec.unsafeWrap(batch._1)
      val target: Vec[Vec[Double, Out], len.type] = Vec.unsafeWrap(batch._2)
      val adjustedStep                            = step / len
      val nnd                                     = calc.LayerOutputs(input, n)
      val next                                    = calc.NextWeights(target, nnd, adjustedStep, 1.0 - inertia, inertia)

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
          case Nil => NoTrainingData(AccGrads.unwrap(n))
          case h :: t =>
            val next               = batchLoop(n, h, t)
            val trainError         = calc.Error(trainTargets, trainInputs, next)
            val nextTestValidation = testValidation.updateCurrentError(next)

            printStatus(
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

    val acc        = AccGrads.wrap(nn)
    val trainError = calc.Error(trainTargets, trainInputs, acc)
    val testValidation: TestValidation[In, Out] = testSamples match {
      case None => NoTestData()
      case Some(s) =>
        TestErrorData(
          currentError          = 0.0,
          bestError             = Double.PositiveInfinity,
          bestAcc               = acc,
          history               = Nil,
          previousMovingAverage = 0.0,
          movingAverageSize     = testMovingAverageSize.getOrElse(1),
          testInputs            = s.map(_.input),
          testTargets           = s.map(_.target)
        ).updateCurrentError(acc)
    }

    printStatus(s"Initial error: $trainError${testValidation.testErrorString}")

    loop(acc, 1, trainError, testValidation)
  }
}
