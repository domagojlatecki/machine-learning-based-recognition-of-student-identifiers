package at.doml.thesis

final case class RgbColor(r: Int, g: Int, b: Int) {
  def toInt: Int = 0xFF000000 | (r << 16) | (g << 8) | b
}

object RgbColor {
  def apply(i: Int): RgbColor = RgbColor((i & 0xFF0000) >>> 16, (i & 0xFF00) >>> 8, i & 0xFF)
}
