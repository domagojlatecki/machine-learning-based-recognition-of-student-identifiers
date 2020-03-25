package at.doml.thesis

import java.nio.file.{Path, Paths}
import scala.util.{Failure, Success, Try}

object Preprocessor {

  private def grayscale(c: Color): Color = {
    val v = (Math.pow((c.r + c.g + c.b) / 768.0, 2.0) * 256.0).toInt
    Color(c.a, v & 0xFF, v & 0xFF, v & 0xFF)
  }

  private def intensity(c: Color): Int = c.r + c.g + c.b

  private final case class DebugData(fileName: Path, debugRoot: Path)

  implicit final class CanvasOps(val c: Canvas) extends AnyVal {

    def debugWrite(stepName: String)(implicit dd: Option[DebugData]): Canvas = {
      dd.foreach { case DebugData(fileName, debugRoot) =>
        println(s"[DEBUG] writing [$fileName] - $stepName")
        c.writeTo(debugRoot.resolve(s"debug-$stepName").resolve(fileName))
      }
      c
    }

    def entropy: Int = c.linear.count(_ == Color.Black)
  }

  // TODO refactor
  def apply(path: Path, debugRoot: Option[Path] = None): Try[(Canvas, Int)] = {
    implicit val debugData: Option[DebugData] = debugRoot.map(DebugData(path.getFileName, _))

    val grayscaled = Canvas.fromPath(path)
      .map(grayscale)
      .debugWrite("grayscale")
    val intensities = grayscaled.linear.filter(_.a == 255).map(intensity)
    val minIntensity = intensities.min
    val maxIntensity = intensities.max
    val cutoffPoint = (maxIntensity - minIntensity) / 2.0

    val contrasted = grayscaled.map { c =>
      if (c.a < 255) {
        Color.White
      } else if (intensity(c) > cutoffPoint) {
        Color.White
      } else {
        Color.Black
      }
    }.debugWrite("contrast")

    var pixelized = contrasted

    while (pixelized.entropy > 500) {
      pixelized = pixelized.mapGrouped(2, 2){ block =>
        val sum = block.map {
          case Color.Black => 1
          case _           => 0
        }.sum

        if (sum > 1) Color.Black else Color.White
      }
    }

    pixelized.debugWrite("pixelize")

    val entropy = pixelized.entropy

    if (entropy < 50) {
      Failure(new RuntimeException(s"Image entropy too low: $entropy")) // TODO specific error
    } else {
      Success((pixelized, entropy))
    }
  }

  def main(args: Array[String]): Unit = {
    Paths.get("data/input").toFile.listFiles.foreach{ f =>
      apply(f.toPath, debugRoot = Some(Paths.get("data/"))) match {
        case Success((_, entropy)) => println(s"[INFO] image [$f] preprocessed, entropy: $entropy")
        case Failure(e) => println(s"[ERROR] failed to process image [$f]: ${e.getMessage}")
      }
    }
  }
}
