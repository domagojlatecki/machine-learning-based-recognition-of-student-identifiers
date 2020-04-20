package at.doml.thesis.preprocessing.transform

import at.doml.thesis.preprocessing.KMeansGrouping
import at.doml.thesis.preprocessing.debug.CanvasDebugger
import at.doml.thesis.preprocessing.image.{Canvas, Color, Point}
import at.doml.thesis.util.Vec

object HotspotsTransform {

  def apply(canvas: Canvas, index: Int)(implicit debugger: CanvasDebugger): Vec[Point, 5] = {
    val initHotspotCentroids: Vec[Point, 5] = Vec.tabulate(5) {
      case 0 => Point(canvas.width / 2.0, canvas.height / 2.0)
      case 1 => Point(canvas.width / 2.0, 0.0)
      case 2 => Point(canvas.width / 2.0, canvas.height)
      case 3 => Point(0.0, canvas.height / 2.0)
      case 4 => Point(canvas.width, canvas.height / 2.0)
    }

    val blackPixels = canvas.getAllPointsWithColor(Color.Black)
    val hotspotCentroids = KMeansGrouping.findCentroids(initHotspotCentroids, blackPixels)

    val xMin = hotspotCentroids.minBy(_.x).x
    val yMin = hotspotCentroids.minBy(_.y).y

    val result = hotspotCentroids.map { p =>
      Point(
        (p.x - xMin) / canvas.width,
        (p.y - yMin) / canvas.height
      )
    }

    debugger(
      {
        val hotspotsSet = hotspotCentroids.map(p => (p.x.toInt, p.y.toInt)).underlying.toSet

        canvas.mapPixels { p =>
          if (hotspotsSet.contains((p.x, p.y))) {
            Color.Red
          } else {
            p.color
          }
        }
      },
      "hotspots",
      index
    )

    result
  }
}
