package at.doml.thesis

import java.nio.file.Paths

object Preprocessor {

  def grayscale(c: RgbColor): RgbColor = {
    val v = (Math.pow((c.r + c.g + c.b) / 768.0, 2.0) * 256.0).toInt
    RgbColor(v & 0xFF, v & 0xFF, v & 0xFF)
  }

  def intensity(c: RgbColor): Int = c.r + c.g + c.b

  def main(args: Array[String]): Unit = {
    val grayscaled = Canvas.fromPath(Paths.get("in.png"))
      .map(grayscale)
    val intensities = grayscaled.linear.map(intensity)
    val minIntensity = intensities.min
    val maxIntensity = intensities.max
    val cutoffPoint = (maxIntensity - minIntensity) / 2.0

    grayscaled.map { c =>
      if (intensity(c) > cutoffPoint) RgbColor.White else RgbColor.Black
    }.writeTo(Paths.get("out.png"))
  }
}
