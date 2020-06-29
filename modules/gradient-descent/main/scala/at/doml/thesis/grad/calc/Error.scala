package at.doml.thesis.grad.calc

import at.doml.thesis.grad.internal.AccGrads
import at.doml.thesis.util.collection.sized.Vec
import at.doml.thesis.util.par.Parallel

private[grad] object Error {

  def apply[In <: Int, Out <: Int, N <: Int](
    targets:      Vec[Vec[Double, Out], N],
    inputs:       Vec[Vec[Double, In], N],
    nn:           AccGrads[In, Out]
  )(implicit par: Parallel): Double = {
    val outputs = inputs.parMap(nn.out)
    val tasks = targets.indices.grouped(par.itemsPerThread).map { indices => () =>
      {
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
}
