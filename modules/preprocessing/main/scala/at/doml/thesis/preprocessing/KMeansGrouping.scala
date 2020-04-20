package at.doml.thesis.preprocessing

import at.doml.thesis.preprocessing.image.Point
import at.doml.thesis.util.Vec
import scala.annotation.tailrec
import scala.collection.immutable.ArraySeq

object KMeansGrouping {

  def findCentroids[N <: Int](centroids: Vec[Point, N], points: ArraySeq[Point]): Vec[Point, N] = {

    @tailrec
    def loop(cs: Vec[Point, N]): Vec[Point, N] = {
      val pointsWithClosestGroups = points.map { p =>
        val distances = cs.mapWithIndex((c, i) => (c.distance(p), i))
        val min = distances.minBy(_._1)
        (p, min._2)
      }

      var changed = false
      val nextCentroids = cs.mapWithIndex { (c, i) =>
        val assignedPoints = pointsWithClosestGroups.filter(_._2 == i)

        val nc = if (assignedPoints.isEmpty) {
          c
        } else {
          val x = assignedPoints.map(_._1.x).sum / assignedPoints.length
          val y = assignedPoints.map(_._1.y).sum / assignedPoints.length

          Point(x, y)
        }

        if (nc.distance(c) > 10e-7) {
          changed = true
        }

        nc
      }

      if (changed) loop(nextCentroids) else nextCentroids
    }

    loop(centroids)
  }
}
