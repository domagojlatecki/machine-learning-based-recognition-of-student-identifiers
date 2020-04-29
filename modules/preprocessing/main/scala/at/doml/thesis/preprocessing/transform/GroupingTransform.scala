package at.doml.thesis.preprocessing.transform

import at.doml.thesis.preprocessing.debug.CanvasDebugger
import at.doml.thesis.preprocessing.image.{Canvas, Color, Point}
import at.doml.thesis.util.Vec
import scala.collection.mutable.ListBuffer

object GroupingTransform {

  def apply(
    canvas:     Canvas,
    canvasName: String,
    n:          Int
  )(
    centroids: Vec[Point, n.type]
  )(
    implicit debugger: CanvasDebugger
  ): Vec[Canvas, n.type] = {
    val groupedPixels: Vec[ListBuffer[Point], n.type] = Vec.fill(n)(ListBuffer.empty)

    canvas.pixels.filter(_.color == Color.Black).foreach { p =>
      val coords = Point(p.x, p.y)
      val distances = centroids.map(c => c.distance(coords)).mapWithIndex(Tuple2.apply)
      val min = distances.minBy(_._1)._2

      groupedPixels(min).append(coords)
    }

    groupedPixels.mapWithIndex { (groupPoints, i) =>
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

      debugger(result, "grouping-individual", s"$canvasName-${i.v}")
      result
    }
  }

}
