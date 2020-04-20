package at.doml.thesis.preprocessing.image

final case class Color(value: Int) extends AnyVal {

  def alpha: Int = (value & 0xFF000000) >>> 24

  def red: Int = (value & 0xFF0000) >>> 16

  def green: Int = (value & 0xFF00) >>> 8

  def blue: Int = value & 0xFF
}

object Color {

  val Red: Color = Color(255, 255, 0, 0)
  val Black: Color = Color(255, 0, 0, 0)
  val White: Color = Color(255, 255, 255, 255)

  def apply(a: Int, r: Int, g: Int, b: Int): Color =
    Color(((a & 0xFF) << 24) | ((r & 0xFF) << 16) | ((g & 0xFF) << 8) | (b & 0xFF))
}
