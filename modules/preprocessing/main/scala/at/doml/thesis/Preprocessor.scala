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

    val contrasted = grayscaled.map { c =>
      if (intensity(c) > cutoffPoint) RgbColor.White else RgbColor.Black
    }

    var pixelized = contrasted

    while (pixelized.area > 5000) {
      pixelized = pixelized.mapGrouped(2, 2){ block =>
        val sum = block.map {
          case RgbColor.Black => 1
          case _              => 0
        }.sum

        if (sum > 1) RgbColor.Black else RgbColor.White
      }
    }

    pixelized.writeTo(Paths.get("out.png"))
  }
}
