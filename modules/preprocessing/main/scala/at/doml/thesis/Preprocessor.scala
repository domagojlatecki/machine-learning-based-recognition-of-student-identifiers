package at.doml.thesis

import java.nio.file.{Path, Paths}
import scala.util.{Failure, Success, Try}

object Preprocessor {

  private def grayscale(c: RgbColor): RgbColor = {
    val v = (Math.pow((c.r + c.g + c.b) / 768.0, 2.0) * 256.0).toInt
    RgbColor(v & 0xFF, v & 0xFF, v & 0xFF)
  }

  private def intensity(c: RgbColor): Int = c.r + c.g + c.b

  private final case class DebugData(fileName: Path, debugRoot: Path)

  implicit final class CanvasOps(val c: Canvas) extends AnyVal {

    def debugWrite(stepName: String)(implicit dd: Option[DebugData]): Canvas = {
      dd.foreach { case DebugData(fileName, debugRoot) =>
        println(s"[DEBUG] writing [$fileName] - $stepName")
        c.writeTo(debugRoot.resolve(s"debug-$stepName").resolve(fileName))
      }
      c
    }
  }

  def apply(path: Path, debugRoot: Option[Path] = None): Try[Canvas] = {
    implicit val debugData: Option[DebugData] = debugRoot.map(DebugData(path.getFileName, _))

    val grayscaled = Canvas.fromPath(path)
      .map(grayscale)
      .debugWrite("grayscale")
    val intensities = grayscaled.linear.map(intensity)
    val minIntensity = intensities.min
    val maxIntensity = intensities.max
    val cutoffPoint = (maxIntensity - minIntensity) / 2.0

    val contrasted = grayscaled.map { c =>
      if (intensity(c) > cutoffPoint) RgbColor.White else RgbColor.Black
    }.debugWrite("contrast")

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

    pixelized.debugWrite("pixelize")

    if (pixelized.linear.count(_ == RgbColor.Black) < 50) {
      Failure(new RuntimeException("Too low entropy image"))
    } else {
      Success(pixelized)
    }
  }

  def main(args: Array[String]): Unit = {
    Paths.get("data/input").toFile.listFiles.foreach{ f =>
      apply(f.toPath, debugRoot = Some(Paths.get("data/")))
    }
  }
}
