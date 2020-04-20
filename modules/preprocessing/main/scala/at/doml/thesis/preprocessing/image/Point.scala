package at.doml.thesis.preprocessing.image

final case class Point(x: Double, y: Double) {

  def distance(that: Point): Double =
    math.sqrt(math.pow(this.x - that.x, 2.0) + math.pow(this.y - that.y, 2.0))
}
