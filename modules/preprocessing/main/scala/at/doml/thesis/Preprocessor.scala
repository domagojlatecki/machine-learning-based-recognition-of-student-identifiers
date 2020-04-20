package at.doml.thesis

import java.nio.file.{Path, Paths}
import scala.collection.immutable.ArraySeq

@deprecated
object Preprocessor {

  private val Colors = ArraySeq(
    Color(255, 255, 0, 0),
    Color(255, 0, 255, 0),
    Color(255, 0, 0, 255),
    Color(255, 255, 255, 0),
    Color(255, 0, 255, 255),
    Color(255, 255, 0, 255),
    Color(255, 128, 64, 64),
    Color(255, 64, 128, 64),
    Color(255, 64, 64, 128),
    Color(255, 128, 64, 128)
  )

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
  }

  private def apply(path: Path, debugRoot: Option[Path] = None): List[LabeledData] = {
    implicit val debugData: Option[DebugData] = debugRoot.map(DebugData(path.getFileName, _))

    val grayscaled = Canvas.fromPath(path)
      .map(grayscale)
      .debugWrite("grayscale")
    val intensities = grayscaled.linear.filter(_.a == 255).map(intensity)
    val minIntensity = intensities.min
    val maxIntensity = intensities.max
    val cutoffPoint = (maxIntensity - minIntensity) / 2.0

    val contrasted = grayscaled .map { c =>
      if (c.a < 255) {
        Color.White
      } else if (intensity(c) > cutoffPoint) {
        Color.White
      } else {
        Color.Black
      }
    }.debugWrite("contrast")

    //
    val trim = contrasted.pixels.groupBy(_.x).toList.sortBy(_._1).map { case (x, pixels) =>
      pixels.map(_.color).reduce { (a, b) => (a, b) match {
          case (Color.Black, _) => Color.Black
          case (_, Color.Black) => Color.Black
          case _                => Color.White
        }
      }
    }
    val left = trim.takeWhile(_ == Color.White).size
    val right = trim.reverse.takeWhile(_ == Color.White).size
    //

    val initCentroids = (0 until 10).map { i =>
      (
        left + (contrasted.width - left - right) / 10.0 * ((2 * i + 1) / 2.0),
        contrasted.height / 2.0
      )
    }.toList
    val blackPixels = contrasted.pixels.filter(_.color == Color.Black)
    val centroids: List[(Double, Double)] = kMeans(initCentroids, blackPixels)

    val grouped = contrasted.mapPixels { p =>
      if (p.color == Color.White) {
        Color.White
      } else {
        val coords = (p.x.toDouble, p.y.toDouble)
        val distances = centroids.map(c => distance(c, coords)).zipWithIndex
        val min = distances.minBy(_._1)
        Colors(min._2)
      }
    }.debugWrite("grouped")

    val data = for (i <- Colors.indices) yield {
      val dd = debugData.map { parent =>
        val name = parent.fileName.toString.replace(".png", s"-$i.png")
        parent.copy(fileName = Paths.get(name))
      }

      val groupColor = Colors(i)
      val groupPixels = grouped.pixels.filter(_.color == groupColor)
      val xs = groupPixels.map(_.x)
      val ys = groupPixels.map(_.y)
      val xMin = xs.min
      val xMax = xs.max
      val yMin = ys.min
      val yMax = ys.max
      val width = xMax - xMin
      val height = yMax - yMin
      val map = groupPixels.map(p => (p.x - xMin, p.y - yMin) -> p.color).toMap

      val c = Canvas.blank(width, height).mapPixels { p =>
        map.getOrElse((p.x, p.y), p.color)
      }

      val initHotspotCentroids = List(
        (width / 2.0, height / 2.0),
        (width / 2.0, 0.0),
        (width / 2.0, height.toDouble),
        (0.0, height / 2.0),
        (width.toDouble, height / 2.0)
      )
      val hotspotCentroids = kMeans(initHotspotCentroids, c.pixels.filter(_.color == groupColor))
      val intHotspots = hotspotCentroids.map { case (a, b) => (a.toInt, b.toInt)}

      c.mapPixels { p =>
        if (intHotspots.contains((p.x, p.y))) {
          Color(255, 255, 0, 0)
        } else if (p.color == groupColor) {
          Color.Black
        } else {
          Color.White
        }
      }.debugWrite("hotspots")(dd)

      val cxmin = hotspotCentroids.map(_._1).min
      val cymin = hotspotCentroids.map(_._2).min
      val xDiff = width.toDouble
      val yDiff = height.toDouble

      LabeledData(
        path.getFileName.toString.charAt(i).toString.toInt,
        hotspotCentroids.map { case (x, y) =>
          val nx = (x - cxmin) / xDiff
          val ny = (y - cymin) / yDiff
          (nx, ny)
        }
      )
    }

    data.toList
  }

  def distance(a: (Double, Double), b: (Double, Double)): Double = {
    math.sqrt(math.pow(a._1 - b._1, 2.0) + math.pow(a._2 - b._2, 2.0))
  }

  private def kMeans(initCentroids: List[(Double, Double)], pixels: ArraySeq[Pixel]): List[(Double, Double)] = {
    var centroids = initCentroids
    var changed = true

    while (changed) {
      changed = false

      val pixelsWithClosest = pixels.map { p =>
        val coords = (p.x.toDouble, p.y.toDouble)
        val distances = centroids.map(c => distance(c, coords)).zipWithIndex
        val min = distances.minBy(_._1)
        (p, min._2)
      }

      val nextCentroids = centroids.zipWithIndex.map { case (c, i) =>
        val assignedPixels = pixelsWithClosest.filter(_._2 == i)
        val len = assignedPixels.length

        val (newX, newY) = if (len == 0) {
          (
            c._1, c._2
          )
        } else {
          (
            assignedPixels.map(_._1.x).sum.toDouble / len,
            assignedPixels.map(_._1.y).sum.toDouble / len
          )
        }


        if ((newX - c._1) > 10e-7 || (newY - c._2) > 10e-7) {
          changed = true
        }

        (newX, newY)
      }

      centroids = nextCentroids
    }

    centroids
  }

  private val ExcludedFiles: Set[String] = Set(
    "0000000000-Pencil-2.png",
    "0036478777-Black_Pen-2.png",
    "0036478777-Green_Pen-2.png",
    "0036478777-Pencil-2.png",
    "0036478777-Red_Pen-2.png",
    "1234567890-Black_Pen-1.png",
    "7777777777-Black_Pen-2.png",
    "0036478777-Green_Pen-1.png"
  )

  def loadDataset(): List[LabeledData] = {
    Paths.get("data/input").toFile
      .listFiles
      .filter(f => !ExcludedFiles.contains(f.getName))
      .flatMap { f =>
        val data = apply(f.toPath)
        val labels = data.map(_.label).mkString(" ")
        println(s"[INFO] image [$f] preprocessed, labels: $labels")
        data
      }
      .toList
  }

  def loadValidation(): Map[String, List[LabeledData]] = {
    Paths.get("data/input").toFile
      .listFiles
      .filter(f => ExcludedFiles.contains(f.getName))
      .map { f =>
        val data = apply(f.toPath)
        val labels = data.map(_.label).mkString(" ")
        println(s"[INFO] image [$f] preprocessed, labels: $labels")
        (f.getName, data)
      }.toMap
  }

  def main(args: Array[String]): Unit = {
    Paths.get("data/input").toFile.listFiles
      .filter(f => !ExcludedFiles.contains(f.getName))
      .foreach { f =>
        val data = apply(f.toPath, debugRoot = Some(Paths.get("data/")))
        val labels = data.map(_.label).mkString(" ")
        println(s"[INFO] image [$f] preprocessed, labels: $labels")
      }
  }
}
