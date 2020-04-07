package at.doml.thesis.nn

final case class Neuron[In <: Int](w: Vec[Double, In], w0: Double) {

  def out(in: Vec[Double, In]): Double = {
    val wArr = w.underlying
    val inArr = in.underlying

    var sum = 0.0
    for (i <- wArr.indices) {
      sum += inArr(i) * wArr(i)
    }

    val net = sum + w0

    1.0 / (1.0 + math.exp(-net))
  }
}

object Neuron {

  def random(size: Int, wRange: (Double, Double)): Neuron[size.type] = {
    val min = wRange._1 min wRange._2
    val max = wRange._1 max wRange._2
    val diff = max - min

    def rand(): Double = max - math.random() * diff

    Neuron(Vec.fill(size)(rand()), rand())
  }
}
