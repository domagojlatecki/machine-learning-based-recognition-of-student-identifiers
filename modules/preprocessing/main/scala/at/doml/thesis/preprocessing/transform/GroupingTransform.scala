package at.doml.thesis.preprocessing.transform

import at.doml.thesis.preprocessing.debug.CanvasDebugger
import at.doml.thesis.preprocessing.image.{Canvas, Color, Pixel, Point}
import at.doml.thesis.util.{Idx, Vec}
import scala.collection.mutable.ListBuffer
import scala.util.Random

object GroupingTransform {

  def groupColor(i: Int): Color = {
    val random = new Random(i)
    Color(255, random.nextInt(256), random.nextInt(256), random.nextInt(256))
  }

  def apply(
    canvas:     Canvas,
    canvasName: String,
    n:          Int
  )(
    centroids: Vec[Point, n.type]
  )(
    implicit debugger: CanvasDebugger
  ): Vec[Canvas, n.type] = {
    def closestGroup(p: Pixel): Idx[n.type] = {
      val coords = Point(p.x, p.y)
      val distances = centroids.map(c => c.distance(coords)).mapWithIndex(Tuple2.apply)
      distances.minBy(_._1)._2
    }

    val groupedPixels: Vec[ListBuffer[Point], n.type] = Vec.fill(n)(ListBuffer.empty)

    canvas.pixels.filter(_.color == Color.Black).foreach { p =>
      groupedPixels(closestGroup(p)).append(Point(p.x, p.y))
    }

    debugger(
      {
        canvas.mapPixels { p =>
          if (p.color == Color.Black) {
            groupColor(closestGroup(p).v)
          } else {
            Color.White
          }
        }
      },
      "grouping",
      canvasName
    )

    groupedPixels.map { groupPoints =>
      val points = groupPoints.toList
      val xs = points.map(_.x.toInt)
      val ys = points.map(_.y.toInt)
      val xMin = xs.min
      val xMax = xs.max
      val yMin = ys.min
      val yMax = ys.max
      val width = xMax - xMin
      val height = yMax - yMin
      val pointsSet = points.map(p => ((p.x - xMin).toInt, (p.y - yMin).toInt)).toSet

      val result = Canvas.blank(width, height).mapPixels { p =>
        if (pointsSet.contains((p.x, p.y))) {
          Color.Black
        } else {
          Color.White
        }
      }

      result
    }
  }

}
