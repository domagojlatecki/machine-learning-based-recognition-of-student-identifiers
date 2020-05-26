package at.doml.thesis.preprocessing.transform

import at.doml.thesis.preprocessing.debug.CanvasDebugger
import at.doml.thesis.preprocessing.image.{Canvas, Color, Pixel}
import at.doml.thesis.util.collection.sized.Vec
import scala.annotation.tailrec
import scala.collection.immutable.ArraySeq
import scala.collection.mutable.ArrayBuffer

// TODO refactor
object HistogramGroupingTransform {

  private implicit final class Tuple2Ops[A, B](val tuple2: (A, B)) extends AnyVal {
    def :+[C](c: C): (A, B, C) = (tuple2._1, tuple2._2, c)
  }

  private val Colors: Map[Int, Color] = Map(
    0 -> Color(255, 200, 200, 200),
    1 -> Color(255, 185, 185, 185),
    2 -> Color(255, 170, 170, 170),
    3 -> Color(255, 155, 155, 155),
    4 -> Color(255, 140, 140, 140),
    5 -> Color(255, 125, 125, 125),
    6 -> Color(255, 110, 110, 110),
    7 -> Color(255, 95, 95, 95),
    8 -> Color(255, 80, 80, 80),
    9 -> Color(255, 65, 65, 65)
  )

  private final case class PartialGroup(min: Int, max: Int, blackPixels: Int)

  private final case class Group(top: Int, bottom: Int, left: Int, right: Int) {
    def width: Int = right - left
    def height: Int = bottom - top
  }

  private def calcTopBottom(group: PartialGroup, canvas: Canvas): (Group, Set[Int]) = {

    def countInRow(row: Int): Int = {
      var sum = 0

      for (x <- group.min until group.max) {
        if (canvas.get(x, row) == Color.Black) {
          sum += 1
        }
      }

      sum
    }

    @tailrec
    def loop(top: Int, bottom: Int, count: Int, moveTop: Boolean, moveBottom: Boolean): (Int, Int) = {
      val topCount = if (moveTop && top - 1 >= 0) countInRow(top - 1) else 0
      val bottomCount = if (moveBottom && bottom + 1 < canvas.height) countInRow(bottom + 1) else 0
      val newCount = count + topCount + bottomCount

      if (newCount.toDouble / group.blackPixels >= 0.99) {
        (top, bottom)
      } else if (topCount > 0 && bottomCount == 0) {
        loop(top - 1, bottom, newCount, moveTop = true, moveBottom = false)
      } else if (topCount == 0 && bottomCount > 0) {
        loop(top, bottom + 1, newCount, moveTop = false, moveBottom = true)
      } else {
        loop(top - 1, bottom + 1, newCount, moveTop = true, moveBottom = true)
      }
    }

    var sum = 0
    var count = 0

    for (y <- 0 until canvas.height) {

      for (x <- group.min until group.max) {
        if (canvas.get(x, y) == Color.Black) {
          sum += y
          count += 1
        }
      }
    }

    if (count > 0) {
      val yMiddle = sum / count
      val (top, bottom) = loop(yMiddle, yMiddle, countInRow(yMiddle), moveTop = true, moveBottom = true)
      val extraHeight = (bottom - top) / 10
      val adjustedTop = (top - extraHeight) max 0
      val adjustedBottom = (bottom + extraHeight) min canvas.height

      (
        Group(top = adjustedTop, bottom = adjustedBottom, left = group.min, right = group.max),
        Set(adjustedTop, adjustedBottom)
      )
    } else {
      (
        Group(top = 0, bottom = canvas.height, left = group.min, right = group.max),
        Set(0, canvas.height)
      )
    }
  }

  private def findGroups(histogram: Map[Int, Int], max: Int): (List[PartialGroup], Set[Int]) = {
    val zeroRegions = histogram.filter(_._2 == 0).keys.toList.sorted

    val zeroCenters = zeroRegions match {

      case Nil => Nil

      case first :: rest =>
        var regionCenters: List[Int] = Nil
        var sum = 0
        var count = 1
        var previous = first

        for (current <- rest) {
          if (previous + 1 == current) {
            sum += current
            count += 1
          } else {
            regionCenters = (sum / count) :: regionCenters
            sum = current
            count = 1
          }

          previous = current
        }

        if (count != 0) {
          regionCenters = (sum / count) :: regionCenters
        }

        0 :: (max :: regionCenters).reverse
    }

    val groups = zeroCenters.sliding(2)
      .map { case a :: b :: Nil => PartialGroup(a, b, histogram.view.filterKeys(k => k >= a && k <= b).values.sum)}
      .toList

    (groups, zeroCenters.toSet)
  }

  def apply(
    canvas:     Canvas,
    canvasName: String,
    n:          Int
  )(implicit debugger: CanvasDebugger): Option[Vec[Canvas, n.type]] = {
    val histogram = canvas.pixels
      .groupBy(_.x)
      .map { case (x, pixels) => (x, pixels.count(_.color == Color.Black)) }
    val (partialGroups, zeroCenters) = findGroups(histogram, canvas.width)
    val groups = partialGroups
      .filter(_.blackPixels > 0)
      .filter(g => g.max - g.min > 0)
      .sortBy(_.blackPixels)(Ordering[Int].reverse)
      .take(n)
      .sortBy(_.min)
      .map(g => calcTopBottom(g, canvas))
      .filter(_._1.height > 0)
      .zipWithIndex
      .map(g => g._1 :+ g._2)
    val groupPixels = groups.map(g => (g._1, ArrayBuffer.empty[Color])).toMap

    def groupFor(p: Pixel): Option[(Group, Set[Int], Color)] = {
      groups.find(g => p.x >= g._1.left && p.x < g._1.right && p.y >= g._1.top && p.y < g._1.bottom)
        .map(g => (g._1, g._2, Colors(g._3)))
    }

    val histogramCanvas = canvas.mapPixels { p =>
      val groupForPixel = groupFor(p)

      groupForPixel.foreach(g => groupPixels(g._1).append(p.color))

      (zeroCenters.contains(p.x), histogram(p.x), groupForPixel) match {
        case (true, _, _)                              => Color.Blue
        case (_, y, _)       if p.y < y                => Color.Red
        case (_, _, Some(g)) if g._2.contains(p.y)     => Color.Green
        case (_, _, Some(g)) if p.color == Color.White => g._3
        case _                                         => p.color
      }
    }

    debugger(histogramCanvas, "histogram", canvasName)

    val out = groups.zipWithIndex.map { case ((group, _, _), i) =>
      val pixels = groupPixels(group).toArray
      val groupCanvas = Canvas(ArraySeq.unsafeWrapArray(pixels), group.width, group.height)

      debugger(groupCanvas, "histogram-group", s"${canvasName}_$i.png")

      groupCanvas
    }

    if (out.length != n) {
      println(s"Cannot form enough groups for canvas: $canvasName")
      None
    } else {
      Some(Vec.unsafeWrap(out.to(ArraySeq)))
    }
  }
}
