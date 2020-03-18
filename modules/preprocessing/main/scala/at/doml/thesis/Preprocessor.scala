package at.doml.thesis

import java.nio.file.Path
import scala.util.{Failure, Success, Try}

object Preprocessor {

  def grayscale(c: RgbColor): RgbColor = {
    val v = (Math.pow((c.r + c.g + c.b) / 768.0, 2.0) * 256.0).toInt
    RgbColor(v & 0xFF, v & 0xFF, v & 0xFF)
  }

  def intensity(c: RgbColor): Int = c.r + c.g + c.b

  def apply(path: Path): Try[Canvas] = {
    val grayscaled = Canvas.fromPath(path)
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

    if (pixelized.linear.count(_ == RgbColor.Black) < 50) {
      Failure(new RuntimeException("Too low entropy image"))
    } else {
      Success(pixelized)
    }
  }
}
