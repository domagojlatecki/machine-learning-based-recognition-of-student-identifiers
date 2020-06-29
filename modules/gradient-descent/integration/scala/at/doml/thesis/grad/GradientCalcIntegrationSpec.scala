package at.doml.thesis.grad

import at.doml.thesis.nn.NeuralNetwork
import at.doml.thesis.util.collection.sized.Vec
import at.doml.thesis.util.par.Parallel
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should
import scala.collection.immutable.ArraySeq

final class GradientCalcIntegrationSpec extends AnyFreeSpec with should.Matchers {

  private implicit object TestParallel extends Parallel {
    override def itemsPerThread: Int = Int.MaxValue
    override def execute[A](tasks: Iterator[() => A]): List[A] = tasks.map(_.apply()).toList
    override def shutdown(): Unit = ()
  }

  private def createSimpleSamples(n: Int): Vec[Sample[1, 1], n.type] = {
    Vec.unsafeWrap[Sample[1, 1], n.type](ArraySeq.tabulate(n) { i =>
      Sample(
        Vec.unsafeWrap[Double, 1](ArraySeq(i.toDouble / n)),
        Vec.unsafeWrap[Double, 1](ArraySeq(1.0 / (1.0 + math.exp(-i.toDouble / n))))
      )
    })
  }

  private def createComplexSamples(n1: Int, n2: Int, n3: Int, n: Int): Vec[Sample[3, 6], n.type] = {
    (n1 * n2 * n3) shouldBe n
    Vec.unsafeWrap[Sample[3, 6], n.type](
      ArraySeq
        .tabulate(n1, n2, n3) {
          case (i, j, k) =>
            Sample(
              Vec.unsafeWrap[Double, 3](ArraySeq(i.toDouble / n, j.toDouble / n, k.toDouble / n)),
              Vec.unsafeWrap[Double, 6](
                ArraySeq(
                  1.0 / (1.0 + math.exp(-i.toDouble / n)),
                  1.0 / (1.0 + math.exp(-j.toDouble / n)),
                  1.0 / (1.0 + math.exp(-k.toDouble / n)),
                  1.0 / (1.0 + math.exp(-(i + j).toDouble / n)),
                  1.0 / (1.0 + math.exp(-(j + k).toDouble / n)),
                  1.0 / (1.0 + math.exp(-(k + i).toDouble / n))
                )
              )
            )
        }.flatten.flatten
    )
  }

  private def checkResult[I <: Int, O <: Int, N <: Int](
    result:    Result[I, O],
    samples:   Vec[Sample[I, O], N],
    tolerance: Double = 0.01
  ): Unit = {
    samples.underlying.foreach { sample =>
      result.nn.out(sample.input).mapWith(sample.target) {
        case (y, t) =>
          y shouldBe t +- tolerance
      }
    }
  }

  "GradientCalc" - {

    "(when not using inertia)" - {

      "should correctly train some NeuralNetwork" - {

        "for some logistic function" - {

          "[with one input parameter]" - {

            "using all samples at once" - {

              "without test data" in {
                val nn: NeuralNetwork[1, 1]         = NeuralNetwork.random(1, List(5), 1, (0.5, 0.5))
                val samples: Vec[Sample[1, 1], 100] = createSimpleSamples(100)
                val result: Result[1, 1] = GradientCalc.optimize(nn)(
                  trainSamples          = samples,
                  testSamples           = None,
                  testMovingAverageSize = None,
                  step                  = 10.0,
                  inertia               = 0.0,
                  batchSize             = BatchSize.all,
                  maxIters              = 1000,
                  targetError           = 10e-7,
                  printStatus           = _ => ()
                )

                checkResult(result, samples)
              }

              "with test data" in {
                val nn: NeuralNetwork[1, 1]              = NeuralNetwork.random(1, List(5), 1, (0.5, 0.5))
                val trainSamples: Vec[Sample[1, 1], 100] = createSimpleSamples(100)
                val testSamples: Vec[Sample[1, 1], 33]   = createSimpleSamples(33)
                val result: Result[1, 1] = GradientCalc.optimize(nn)(
                  trainSamples          = trainSamples,
                  testSamples           = Some(testSamples),
                  testMovingAverageSize = Some(5),
                  step                  = 10.0,
                  inertia               = 0.0,
                  batchSize             = BatchSize.all,
                  maxIters              = 1000,
                  targetError           = 10e-7,
                  printStatus           = _ => ()
                )

                checkResult(result, trainSamples)
                checkResult(result, testSamples)
              }
            }

            "using small batches" - {

              "without test data" in {
                val nn: NeuralNetwork[1, 1]         = NeuralNetwork.random(1, List(5), 1, (0.5, 0.5))
                val samples: Vec[Sample[1, 1], 100] = createSimpleSamples(100)
                val result: Result[1, 1] = GradientCalc.optimize(nn)(
                  trainSamples          = samples,
                  testSamples           = None,
                  testMovingAverageSize = None,
                  step                  = 10.0,
                  inertia               = 0.0,
                  batchSize             = BatchSize.of(10),
                  maxIters              = 1000,
                  targetError           = 10e-7,
                  printStatus           = _ => ()
                )

                checkResult(result, samples)
              }

              "with test data" in {
                val nn: NeuralNetwork[1, 1]              = NeuralNetwork.random(1, List(5), 1, (0.5, 0.5))
                val trainSamples: Vec[Sample[1, 1], 100] = createSimpleSamples(100)
                val testSamples: Vec[Sample[1, 1], 33]   = createSimpleSamples(33)
                val result: Result[1, 1] = GradientCalc.optimize(nn)(
                  trainSamples          = trainSamples,
                  testSamples           = Some(testSamples),
                  testMovingAverageSize = Some(10),
                  step                  = 10.0,
                  inertia               = 0.0,
                  batchSize             = BatchSize.of(10),
                  maxIters              = 1000,
                  targetError           = 10e-7,
                  printStatus           = _ => ()
                )

                checkResult(result, trainSamples)
                checkResult(result, testSamples)
              }
            }

            "using one sample per batch" - {

              "without test data" in {
                val nn: NeuralNetwork[1, 1]         = NeuralNetwork.random(1, List(5), 1, (0.5, 0.5))
                val samples: Vec[Sample[1, 1], 100] = createSimpleSamples(100)
                val result: Result[1, 1] = GradientCalc.optimize(nn)(
                  trainSamples          = samples,
                  testSamples           = None,
                  testMovingAverageSize = None,
                  step                  = 10.0,
                  inertia               = 0.0,
                  batchSize             = BatchSize.of(1),
                  maxIters              = 1000,
                  targetError           = 10e-7,
                  printStatus           = _ => ()
                )

                checkResult(result, samples)
              }

              "with test data" in {
                val nn: NeuralNetwork[1, 1]              = NeuralNetwork.random(1, List(5), 1, (0.5, 0.5))
                val trainSamples: Vec[Sample[1, 1], 100] = createSimpleSamples(100)
                val testSamples: Vec[Sample[1, 1], 33]   = createSimpleSamples(33)
                val result: Result[1, 1] = GradientCalc.optimize(nn)(
                  trainSamples          = trainSamples,
                  testSamples           = Some(testSamples),
                  testMovingAverageSize = Some(100),
                  step                  = 10.0,
                  inertia               = 0.0,
                  batchSize             = BatchSize.of(1),
                  maxIters              = 1000,
                  targetError           = 10e-7,
                  printStatus           = _ => ()
                )

                checkResult(result, trainSamples)
                checkResult(result, testSamples)
              }
            }
          }

          "[with multiple input parameters]" - {

            "using all samples at once" - {

              "without test data" in {
                val nn: NeuralNetwork[3, 6]         = NeuralNetwork.random(3, List(4, 5, 6), 6, (0.5, 0.5))
                val samples: Vec[Sample[3, 6], 100] = createComplexSamples(5, 4, 5, 100)
                val result: Result[3, 6] = GradientCalc.optimize(nn)(
                  trainSamples          = samples,
                  testSamples           = None,
                  testMovingAverageSize = None,
                  step                  = 10.0,
                  inertia               = 0.0,
                  batchSize             = BatchSize.all,
                  maxIters              = 1000,
                  targetError           = 10e-7,
                  printStatus           = _ => ()
                )

                checkResult(result, samples)
              }

              "with test data" in {
                val nn: NeuralNetwork[3, 6]              = NeuralNetwork.random(3, List(4, 5, 6), 6, (0.65, 0.65))
                val trainSamples: Vec[Sample[3, 6], 100] = createComplexSamples(5, 4, 5, 100)
                val testSamples: Vec[Sample[3, 6], 180]  = createComplexSamples(6, 5, 6, 180)
                val result: Result[3, 6] = GradientCalc.optimize(nn)(
                  trainSamples          = trainSamples,
                  testSamples           = Some(testSamples),
                  testMovingAverageSize = Some(25),
                  step                  = 5.0,
                  inertia               = 0.0,
                  batchSize             = BatchSize.all,
                  maxIters              = 1000,
                  targetError           = 10e-7,
                  printStatus           = _ => ()
                )

                checkResult(result, trainSamples, tolerance = 0.05)
                checkResult(result, testSamples, tolerance  = 0.05)
              }
            }

            "using small batches" - {

              "without test data" in {
                val nn: NeuralNetwork[3, 6]         = NeuralNetwork.random(3, List(4, 5, 6), 6, (0.5, 0.5))
                val samples: Vec[Sample[3, 6], 100] = createComplexSamples(5, 4, 5, 100)
                val result: Result[3, 6] = GradientCalc.optimize(nn)(
                  trainSamples          = samples,
                  testSamples           = None,
                  testMovingAverageSize = None,
                  step                  = 10.0,
                  inertia               = 0.0,
                  batchSize             = BatchSize.of(10),
                  maxIters              = 1000,
                  targetError           = 10e-7,
                  printStatus           = _ => ()
                )

                checkResult(result, samples, tolerance = 0.05)
              }

              "with test data" in {
                val nn: NeuralNetwork[3, 6]              = NeuralNetwork.random(3, List(4, 5, 6), 6, (0.5, 0.5))
                val trainSamples: Vec[Sample[3, 6], 100] = createComplexSamples(5, 4, 5, 100)
                val testSamples: Vec[Sample[3, 6], 180]  = createComplexSamples(6, 5, 6, 180)
                val result: Result[3, 6] = GradientCalc.optimize(nn)(
                  trainSamples          = trainSamples,
                  testSamples           = Some(testSamples),
                  testMovingAverageSize = Some(1),
                  step                  = 10.0,
                  inertia               = 0.0,
                  batchSize             = BatchSize.of(10),
                  maxIters              = 1000,
                  targetError           = 10e-7,
                  printStatus           = _ => ()
                )

                checkResult(result, trainSamples, tolerance = 0.05)
                checkResult(result, testSamples, tolerance  = 0.05)
              }
            }

            "using one sample per batch" - {

              "without test data" in {
                val nn: NeuralNetwork[3, 6]         = NeuralNetwork.random(3, List(4, 5, 6), 6, (0.5, 0.5))
                val samples: Vec[Sample[3, 6], 100] = createComplexSamples(5, 4, 5, 100)
                val result: Result[3, 6] = GradientCalc.optimize(nn)(
                  trainSamples          = samples,
                  testSamples           = None,
                  testMovingAverageSize = None,
                  step                  = 10.0,
                  inertia               = 0.0,
                  batchSize             = BatchSize.of(1),
                  maxIters              = 1000,
                  targetError           = 10e-7,
                  printStatus           = _ => ()
                )

                checkResult(result, samples, tolerance = 0.05)
              }

              "with test data" in {
                val nn: NeuralNetwork[3, 6]              = NeuralNetwork.random(3, List(4, 5, 6), 6, (0.5, 0.5))
                val trainSamples: Vec[Sample[3, 6], 100] = createComplexSamples(5, 4, 5, 100)
                val testSamples: Vec[Sample[3, 6], 180]  = createComplexSamples(6, 5, 6, 180)
                val result: Result[3, 6] = GradientCalc.optimize(nn)(
                  trainSamples          = trainSamples,
                  testSamples           = Some(testSamples),
                  testMovingAverageSize = Some(10),
                  step                  = 2.5,
                  inertia               = 0.0,
                  batchSize             = BatchSize.of(1),
                  maxIters              = 1000,
                  targetError           = 10e-7,
                  printStatus           = _ => ()
                )

                checkResult(result, trainSamples, tolerance = 0.05)
                checkResult(result, testSamples, tolerance  = 0.05)
              }
            }
          }
        }
      }
    }

    "(when using inertia)" - {

      "should correctly train some NeuralNetwork" - {

        "for some logistic function" - {

          "[with one input parameter]" - {

            "using all samples at once" - {

              "without test data" in {
                val nn: NeuralNetwork[1, 1]         = NeuralNetwork.random(1, List(5), 1, (0.5, 0.5))
                val samples: Vec[Sample[1, 1], 100] = createSimpleSamples(100)
                val result: Result[1, 1] = GradientCalc.optimize(nn)(
                  trainSamples          = samples,
                  testSamples           = None,
                  testMovingAverageSize = None,
                  step                  = 10.0,
                  inertia               = 0.5,
                  batchSize             = BatchSize.all,
                  maxIters              = 1000,
                  targetError           = 10e-7,
                  printStatus           = _ => ()
                )

                checkResult(result, samples)
              }

              "with test data" in {
                val nn: NeuralNetwork[1, 1]              = NeuralNetwork.random(1, List(5), 1, (0.5, 0.5))
                val trainSamples: Vec[Sample[1, 1], 100] = createSimpleSamples(100)
                val testSamples: Vec[Sample[1, 1], 33]   = createSimpleSamples(33)
                val result: Result[1, 1] = GradientCalc.optimize(nn)(
                  trainSamples          = trainSamples,
                  testSamples           = Some(testSamples),
                  testMovingAverageSize = Some(5),
                  step                  = 10.0,
                  inertia               = 0.5,
                  batchSize             = BatchSize.all,
                  maxIters              = 1000,
                  targetError           = 10e-7,
                  printStatus           = _ => ()
                )

                checkResult(result, trainSamples)
                checkResult(result, testSamples)
              }
            }

            "using small batches" - {

              "without test data" in {
                val nn: NeuralNetwork[1, 1]         = NeuralNetwork.random(1, List(5), 1, (0.5, 0.5))
                val samples: Vec[Sample[1, 1], 100] = createSimpleSamples(100)
                val result: Result[1, 1] = GradientCalc.optimize(nn)(
                  trainSamples          = samples,
                  testSamples           = None,
                  testMovingAverageSize = None,
                  step                  = 10.0,
                  inertia               = 0.8,
                  batchSize             = BatchSize.of(10),
                  maxIters              = 1000,
                  targetError           = 10e-7,
                  printStatus           = _ => ()
                )

                checkResult(result, samples)
              }

              "with test data" in {
                val nn: NeuralNetwork[1, 1]              = NeuralNetwork.random(1, List(5), 1, (0.5, 0.5))
                val trainSamples: Vec[Sample[1, 1], 100] = createSimpleSamples(100)
                val testSamples: Vec[Sample[1, 1], 33]   = createSimpleSamples(33)
                val result: Result[1, 1] = GradientCalc.optimize(nn)(
                  trainSamples          = trainSamples,
                  testSamples           = Some(testSamples),
                  testMovingAverageSize = Some(10),
                  step                  = 10.0,
                  inertia               = 0.8,
                  batchSize             = BatchSize.of(10),
                  maxIters              = 1000,
                  targetError           = 10e-7,
                  printStatus           = _ => ()
                )

                checkResult(result, trainSamples)
                checkResult(result, testSamples)
              }
            }

            "using one sample per batch" - {

              "without test data" in {
                val nn: NeuralNetwork[1, 1]         = NeuralNetwork.random(1, List(5), 1, (0.5, 0.5))
                val samples: Vec[Sample[1, 1], 100] = createSimpleSamples(100)
                val result: Result[1, 1] = GradientCalc.optimize(nn)(
                  trainSamples          = samples,
                  testSamples           = None,
                  testMovingAverageSize = None,
                  step                  = 10.0,
                  inertia               = 0.25,
                  batchSize             = BatchSize.of(1),
                  maxIters              = 1000,
                  targetError           = 10e-7,
                  printStatus           = _ => ()
                )

                checkResult(result, samples)
              }

              "with test data" in {
                val nn: NeuralNetwork[1, 1]              = NeuralNetwork.random(1, List(5), 1, (0.5, 0.5))
                val trainSamples: Vec[Sample[1, 1], 100] = createSimpleSamples(100)
                val testSamples: Vec[Sample[1, 1], 33]   = createSimpleSamples(33)
                val result: Result[1, 1] = GradientCalc.optimize(nn)(
                  trainSamples          = trainSamples,
                  testSamples           = Some(testSamples),
                  testMovingAverageSize = Some(100),
                  step                  = 10.0,
                  inertia               = 0.25,
                  batchSize             = BatchSize.of(1),
                  maxIters              = 1000,
                  targetError           = 10e-7,
                  printStatus           = _ => ()
                )

                checkResult(result, trainSamples)
                checkResult(result, testSamples)
              }
            }
          }

          "[with multiple input parameters]" - {

            "using all samples at once" - {

              "without test data" in {
                val nn: NeuralNetwork[3, 6]         = NeuralNetwork.random(3, List(4, 5, 6), 6, (0.5, 0.5))
                val samples: Vec[Sample[3, 6], 100] = createComplexSamples(5, 4, 5, 100)
                val result: Result[3, 6] = GradientCalc.optimize(nn)(
                  trainSamples          = samples,
                  testSamples           = None,
                  testMovingAverageSize = None,
                  step                  = 10.0,
                  inertia               = 0.5,
                  batchSize             = BatchSize.all,
                  maxIters              = 1000,
                  targetError           = 10e-7,
                  printStatus           = _ => ()
                )

                checkResult(result, samples)
              }

              "with test data" in {
                val nn: NeuralNetwork[3, 6]              = NeuralNetwork.random(3, List(4, 5, 6), 6, (0.65, 0.65))
                val trainSamples: Vec[Sample[3, 6], 100] = createComplexSamples(5, 4, 5, 100)
                val testSamples: Vec[Sample[3, 6], 180]  = createComplexSamples(6, 5, 6, 180)
                val result: Result[3, 6] = GradientCalc.optimize(nn)(
                  trainSamples          = trainSamples,
                  testSamples           = Some(testSamples),
                  testMovingAverageSize = Some(25),
                  step                  = 5.0,
                  inertia               = 0.5,
                  batchSize             = BatchSize.all,
                  maxIters              = 1000,
                  targetError           = 10e-7,
                  printStatus           = _ => ()
                )

                checkResult(result, trainSamples, tolerance = 0.05)
                checkResult(result, testSamples, tolerance  = 0.05)
              }
            }

            "using small batches" - {

              "without test data" in {
                val nn: NeuralNetwork[3, 6]         = NeuralNetwork.random(3, List(4, 5, 6), 6, (0.5, 0.5))
                val samples: Vec[Sample[3, 6], 100] = createComplexSamples(5, 4, 5, 100)
                val result: Result[3, 6] = GradientCalc.optimize(nn)(
                  trainSamples          = samples,
                  testSamples           = None,
                  testMovingAverageSize = None,
                  step                  = 10.0,
                  inertia               = 0.8,
                  batchSize             = BatchSize.of(10),
                  maxIters              = 1000,
                  targetError           = 10e-7,
                  printStatus           = _ => ()
                )

                checkResult(result, samples, tolerance = 0.05)
              }

              "with test data" in {
                val nn: NeuralNetwork[3, 6]              = NeuralNetwork.random(3, List(4, 5, 6), 6, (0.5, 0.5))
                val trainSamples: Vec[Sample[3, 6], 100] = createComplexSamples(5, 4, 5, 100)
                val testSamples: Vec[Sample[3, 6], 180]  = createComplexSamples(6, 5, 6, 180)
                val result: Result[3, 6] = GradientCalc.optimize(nn)(
                  trainSamples          = trainSamples,
                  testSamples           = Some(testSamples),
                  testMovingAverageSize = Some(1),
                  step                  = 10.0,
                  inertia               = 0.8,
                  batchSize             = BatchSize.of(10),
                  maxIters              = 1000,
                  targetError           = 10e-7,
                  printStatus           = _ => ()
                )

                checkResult(result, trainSamples, tolerance = 0.05)
                checkResult(result, testSamples, tolerance  = 0.05)
              }
            }

            "using one sample per batch" - {

              "without test data" in {
                val nn: NeuralNetwork[3, 6]         = NeuralNetwork.random(3, List(4, 5, 6), 6, (0.5, 0.5))
                val samples: Vec[Sample[3, 6], 100] = createComplexSamples(5, 4, 5, 100)
                val result: Result[3, 6] = GradientCalc.optimize(nn)(
                  trainSamples          = samples,
                  testSamples           = None,
                  testMovingAverageSize = None,
                  step                  = 10.0,
                  inertia               = 0.25,
                  batchSize             = BatchSize.of(1),
                  maxIters              = 1000,
                  targetError           = 10e-7,
                  printStatus           = _ => ()
                )

                checkResult(result, samples, tolerance = 0.05)
              }

              "with test data" in {
                val nn: NeuralNetwork[3, 6]              = NeuralNetwork.random(3, List(4, 5, 6), 6, (0.5, 0.5))
                val trainSamples: Vec[Sample[3, 6], 100] = createComplexSamples(5, 4, 5, 100)
                val testSamples: Vec[Sample[3, 6], 180]  = createComplexSamples(6, 5, 6, 180)
                val result: Result[3, 6] = GradientCalc.optimize(nn)(
                  trainSamples          = trainSamples,
                  testSamples           = Some(testSamples),
                  testMovingAverageSize = Some(10),
                  step                  = 2.5,
                  inertia               = 0.25,
                  batchSize             = BatchSize.of(1),
                  maxIters              = 1000,
                  targetError           = 10e-7,
                  printStatus           = _ => ()
                )

                checkResult(result, trainSamples, tolerance = 0.05)
                checkResult(result, testSamples, tolerance  = 0.05)
              }
            }
          }
        }
      }
    }
  }
}
