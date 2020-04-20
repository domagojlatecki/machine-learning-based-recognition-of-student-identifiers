package at.doml.thesis.preprocessing

import at.doml.thesis.preprocessing.image.Point
import at.doml.thesis.util.Vec

sealed trait Data

object Data {

  final case class Raw(hotspots: Vec[Point, 5]) extends Data

  final case class Labeled(hotspots: Vec[Point, 5], label: Int) extends Data
}
