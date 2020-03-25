package at.doml.thesis

final case class Color(a: Int, r: Int, g: Int, b: Int) {
  def toInt: Int = (a << 24) | (r << 16) | (g << 8) | b
}

object Color {

  val Black: Color = Color(255, 0, 0, 0)
  val White: Color = Color(255, 255, 255, 255)

  def apply(i: Int): Color = {
    Color((i & 0xFF000000) >>> 24, (i & 0xFF0000) >>> 16, (i & 0xFF00) >>> 8, i & 0xFF)
  }
}
