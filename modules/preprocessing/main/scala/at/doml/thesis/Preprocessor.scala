package at.doml.thesis

import java.nio.file.{Path, Paths}
import scala.util.{Failure, Random, Success, Try}

// TODO refactor
object Preprocessor {

  private final val FileNameRegex = "([0-9]{10})-(.+)".r

  private def grayscale(c: Color): Color = {
    val v = (Math.pow((c.r + c.g + c.b) / 768.0, 2.0) * 256.0).toInt
    Color(c.a, v & 0xFF, v & 0xFF, v & 0xFF)
  }

  private def intensity(c: Color): Int = c.r + c.g + c.b

  private final case class DebugData(fileName: Path, debugRoot: Path)

  implicit final class CanvasOps(val c: Canvas) extends AnyVal {

    def debugWrite(stepName: String)(implicit dd: Option[DebugData]): Canvas = {
      dd.foreach { case DebugData(fileName, debugRoot) =>
        println(s"[DEBUG] writing [$fileName] - $stepName (${c.entropy} black pixels)")
        c.writeTo(debugRoot.resolve(s"debug-$stepName").resolve(fileName))
      }
      c
    }

    def entropy: Int = c.linear.count(_ == Color.Black)
  }

  private def columnGroups(canvas: Canvas): List[(Int, Int)] = {
    val colWidth = canvas.width / 10.0
    lazy val cols: LazyList[Double] = 0 #:: colWidth #:: cols.zip(cols.tail).map { n => n._1 + colWidth }
    val groups = cols.grouped(2)
      .take(10)
      .map(_.toList)
      .map {
        case List(a, b) => (a.toInt, b.toInt)
      }.toList
    val updated = groups.updated(9, (groups(9)._1, canvas.width)) // make sure that entire width is used
    updated.sliding(2).map(_.flatMap { case (a, b) => List(a, b) } ).map(l => (l.min, l.max)).toList
  }

  def spacing(pixels: Seq[(Int, Int)]): Double = {
    val distances = for {
      p1 <- pixels
      p2 <- pixels
    } yield Math.sqrt(Math.pow(p1._1 - p2._1, 2.0) + Math.pow(p1._2 - p2._2, 2.0))

    distances.sum / distances.length
  }

  def apply(path: Path, debugRoot: Option[Path] = None): List[LabeledData] = {
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

    val FileNameRegex(digits, name) = path.getFileName.toString
    val columns = columnGroups(pixelized)
    val digitGroups = digits.toSeq
      .sliding(2)
      .zipWithIndex.map { case (d, i) => s"${"_" * i}$d${"_" * (8 - i)}"}
      .toList

    val groups = for ((column, digitGroup) <- columns zip digitGroups) yield {
      pixelized.fromColumns(column._1, column._2)
        .debugWrite("group")(debugData.map(_.copy(fileName = Paths.get(s"$digitGroup-$name"))))
    }

    val randomized = for ((group, digitGroup) <- groups zip digitGroups) yield {
      val blackPixels = group.pixels.filter(_.color == Color.Black)

      if (blackPixels.length < 20) {
        new RuntimeException(s"Image entropy too low: ${blackPixels.length}") // TODO handle this better!
      }

      var bestSpacedPixels = blackPixels.take(20).map(p => (p.x, p.y))
      var bestSpacing = spacing(bestSpacedPixels)

      for (_ <- 0 until 1000) {
        val randomPixels = Random.shuffle(blackPixels).take(20).map(p => (p.x, p.y))
        val randomSpacing = spacing(randomPixels)

        if (randomSpacing > bestSpacing) {
          bestSpacedPixels = randomPixels
          bestSpacing = randomSpacing
        }
      }

      val evenlySpacedPixels = bestSpacedPixels.toSet
      group.mapPixels { p =>
        if (evenlySpacedPixels.contains((p.x, p.y))) {
          p.color
        } else {
          Color.White
        }
      }.debugWrite("randomized")(debugData.map(_.copy(fileName = Paths.get(s"$digitGroup-$name"))))
    }

    val imageData = for ((group, digitGroup) <- randomized zip digitGroups) yield {
      val twoDigitLabel = digitGroup.replaceAll("_", "")
      val label = (twoDigitLabel(0).toString.toByte, twoDigitLabel(1).toString.toByte)
      val blackPixels = group.pixels.filter(_.color == Color.Black)

      val xMin = blackPixels.map(_.x).min
      val xMax = blackPixels.map(_.x).max
      val xDiff = (xMax - xMin).toDouble

      val yMin = blackPixels.map(_.y).min
      val yMax = blackPixels.map(_.y).max
      val yDiff = (yMax - yMin).toDouble

      val points = blackPixels.map { p =>
        val x = (p.x - xMin) / xDiff
        val y = (p.y - yMin) / yDiff
        (x, y)
      }

      LabeledData(label, points.toList)
    }

    imageData
  }

  def main(args: Array[String]): Unit = {
    Paths.get("data/input").toFile.listFiles.foreach { f =>
      apply(f.toPath, debugRoot = Some(Paths.get("data/"))) match {
        case _ => println(s"[INFO] image [$f] preprocessed")
      }
    }
  }
}
