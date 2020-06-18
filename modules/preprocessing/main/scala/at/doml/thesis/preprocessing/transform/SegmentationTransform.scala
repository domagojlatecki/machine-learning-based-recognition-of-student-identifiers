package at.doml.thesis.preprocessing.transform

import at.doml.thesis.preprocessing.debug.CanvasDebugger
import at.doml.thesis.preprocessing.image.{Canvas, Color, Pixel}
import at.doml.thesis.util.collection.sized.Vec
import scala.annotation.tailrec
import scala.collection.immutable.ArraySeq

object SegmentationTransform {

  private final case class PixelGroup(pixels: Set[Pixel]) {

    lazy val leftmostCoord: Int = pixels.map(_.x).min
    lazy val topmostCoord: Int = pixels.map(_.y).min

    lazy val width: Int = {
      val xs = pixels.map(_.x)
      val xMin = xs.min
      val xMax = xs.max

      xMax - xMin
    }

    lazy val height: Int = {
      val ys = pixels.map(_.y)
      val yMin = ys.min
      val yMax = ys.max

      yMax - yMin
    }

    def centroid: (Int, Int) = {
      val xs = pixels.map(_.x)
      val ys = pixels.map(_.y)
      val centerX = xs.sum.toDouble / xs.size
      val centerY = ys.sum.toDouble / ys.size

      (centerX.toInt, centerY.toInt)
    }
  }

  private val Color1 = Color(255, 255, 0, 0)
  private val Color2 = Color(255, 0, 0, 255)
  private val Color3 = Color(255, 0, 255, 0)

  private def color(i: Int, n: Int): Color = {
    if (i < 0 || i >= n) {
      Color.Black
    } else if (i <= (n - 1) / 2) {
      val coeff2 = i.toDouble / ((n - 1) / 2)
      val coeff1 = 1.0 - coeff2

      Color(
        255,
        (Color1.red * coeff1 + Color2.red * coeff2).toInt,
        (Color1.green * coeff1 + Color2.green * coeff2).toInt,
        (Color1.blue * coeff1 + Color2.blue * coeff2).toInt
      )
    } else {
      val coeff3 = (i.toDouble - (n - 1) / 2) / (n / 2)
      val coeff2 = 1.0 - coeff3

      Color(
        255,
        (Color2.red * coeff2 + Color3.red * coeff3).toInt,
        (Color2.green * coeff2 + Color3.green * coeff3).toInt,
        (Color2.blue * coeff2 + Color3.blue * coeff3).toInt
      )
    }
  }

  private def splitGroups(groups: List[PixelGroup], n: Int): List[PixelGroup] = {

    def splitGroupInHalf(group: PixelGroup): (PixelGroup, PixelGroup) = {
      val splitPoint = group.width / 2 + group.leftmostCoord
      val (newGroup1, newGroup2) = group.pixels.partition(_.x < splitPoint)

      (PixelGroup(newGroup1), PixelGroup(newGroup2))
    }

    def splitGroupInThirds(group: PixelGroup): (PixelGroup, PixelGroup, PixelGroup) = {
      val splitPoint1 = group.width / 3 + group.leftmostCoord
      val (newGroup1, rest) = group.pixels.partition(_.x < splitPoint1)

      val restGroup = PixelGroup(rest)
      val splitPoint2 = restGroup.width / 2 + restGroup.leftmostCoord
      val (newGroup2, newGroup3) = restGroup.pixels.partition(_.x < splitPoint2)

      (PixelGroup(newGroup1), PixelGroup(newGroup2), PixelGroup(newGroup3))
    }

    val sortedByWidth = groups.sortBy(_.width)(Ordering[Int].reverse)

    val out = if (groups.size == n - 1) {
      val widestGroup :: rest = sortedByWidth
      val (newGroup1, newGroup2) = splitGroupInHalf(widestGroup)

      newGroup1 :: newGroup2 :: rest
    } else if (groups.size == n - 2) {
      val widestGroup :: secondWidestGroup :: rest = sortedByWidth

      if (widestGroup.width.toDouble / secondWidestGroup.width.toDouble >= 1.33) {
        val (newGroup1, newGroup2, newGroup3) = splitGroupInThirds(widestGroup)

        newGroup1 :: newGroup2 :: newGroup3 :: secondWidestGroup :: rest
      } else {
        val (newGroup1, newGroup2) = splitGroupInHalf(widestGroup)
        val (newGroup3, newGroup4) = splitGroupInHalf(secondWidestGroup)

        newGroup1 :: newGroup2 :: newGroup3 :: newGroup4 :: rest
      }
    } else {
      val numMissing = n - groups.size
      val (toSplit, rest) = sortedByWidth.splitAt(numMissing)
      val splitInfHalf = toSplit.map(splitGroupInHalf).flatMap(t => List(t._1, t._2))

      (splitInfHalf ::: rest).take(10)
    }

    out
  }

  private def mergeGroups(majorGroups: List[PixelGroup], minorGroups: List[PixelGroup]): List[PixelGroup] = {
    val majorGroupsByCentroids = majorGroups.map(g => (g.centroid, g)).toMap
    val centroids = majorGroupsByCentroids.keys.toList
    val minorGroupsByClosestMajorGroup = minorGroups.groupBy { group =>
      val (x1, y1) = group.centroid
      centroids.minBy { case (x2, y2) => math.sqrt(math.pow(x1 - x2, 2.0) + math.pow(y1 - y2, 2.0)) }
    }

    majorGroupsByCentroids.map { case (centroid, group) =>
      val assignedMinorGroups = minorGroupsByClosestMajorGroup.getOrElse(centroid, Nil).flatMap(_.pixels)
      PixelGroup(group.pixels ++ assignedMinorGroups)
    }.toList
  }

  def apply(
    canvas:     Canvas,
    canvasName: String,
    n:          Int
  )(implicit debugger: CanvasDebugger): Option[Vec[Canvas, n.type]] = {
    val blackPixelsByCoords = canvas.getAllPixelsWithColor(Color.Black)
      .map(p => ((p.x, p.y), p))
      .toMap

    @tailrec
    def groupPixels(
      boundaryPixels:  Set[Pixel],
      groupedPixels:   Set[Pixel],
      ungroupedPixels: Map[(Int, Int), Pixel]
    ): (PixelGroup, Map[(Int, Int), Pixel]) = {
      if (boundaryPixels.isEmpty) {
        (PixelGroup(groupedPixels), ungroupedPixels.filterNot(kv => groupedPixels.contains(kv._2)))
      } else {
        val newBoundaryPixels = boundaryPixels.flatMap { p =>
          List(
            ungroupedPixels.get((p.x + 1, p.y)),
            ungroupedPixels.get((p.x - 1, p.y)),
            ungroupedPixels.get((p.x, p.y + 1)),
            ungroupedPixels.get((p.x, p.y - 1))
          ).flatten
        }
        val newGroupedPixels = groupedPixels ++ boundaryPixels
        val newUngroupedPixels = ungroupedPixels.filterNot(kv => newGroupedPixels.contains(kv._2))

        groupPixels(newBoundaryPixels, newGroupedPixels, newUngroupedPixels)
      }
    }

    @tailrec
    def loop(ungroupedPixels: Map[(Int, Int), Pixel], groups: List[PixelGroup]): List[PixelGroup] = {
      if (ungroupedPixels.isEmpty) {
        groups
      } else {
        val (group, newUngroupedPixels) = groupPixels(Set(ungroupedPixels.head._2), Set.empty, ungroupedPixels)

        loop(newUngroupedPixels, group :: groups)
      }
    }

    val threshold = canvas.getAllPixelsWithColor(Color.Black).size / (3 * n)
    val pixelGroups = loop(blackPixelsByCoords, Nil).sortBy(_.pixels.size)(Ordering[Int].reverse)
    val (potentialMajorGroups, potentialMinorGroups) = pixelGroups.partition(_.pixels.size >= threshold)
    val (unsortedMajorGroups, remainingMinorGroups) = potentialMajorGroups.splitAt(n)
    val majorGroups = unsortedMajorGroups
    val minorGroups = potentialMinorGroups ++ remainingMinorGroups

    val correctedGroups = if (majorGroups.size == n) {
      majorGroups
    } else {
      splitGroups(majorGroups, n)
    }

    val sortedGroups = mergeGroups(correctedGroups, minorGroups).sortBy(_.leftmostCoord).zipWithIndex

    debugger(
      canvas.mapPixels { p =>
        if (p.color != Color.Black) {
          p.color
        } else {
          val groupForPixel = sortedGroups.find(pg => pg._1.pixels.contains(p)).map(_._2).getOrElse(-1)
          color(groupForPixel, n)
        }
      },
      "segmentation",
      canvasName
    )

    if (sortedGroups.size == n) {
      val groups = Vec.unsafeWrap[PixelGroup, n.type](sortedGroups.map(_._1).to(ArraySeq))
      val out = groups.map { group =>
        val coords = group.pixels.map(p => (p.x, p.y))

        Canvas.blank(group.width, group.height).mapPixels { p =>
          if (coords.contains((p.x + group.leftmostCoord, p.y + group.topmostCoord))) {
            Color.Black
          } else {
            Color.White
          }
        }
      }

      Some(out)
    } else {
      println(s"Canvas $canvasName has invalid number of groups: ${majorGroups.size} (${minorGroups.size} minor)")
      None
    }
  }
}
