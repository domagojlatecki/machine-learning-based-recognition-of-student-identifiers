package at.doml.thesis.preprocessing

import at.doml.thesis.util.Vec

sealed trait Data extends Product with Serializable {
  val features: Vec[Double, Data.NumFeatures.type]
}

object Data {

  val NumFeatures: Int = 24

  final case class Raw(features: Vec[Double, NumFeatures.type]) extends Data

  final case class Labeled(features: Vec[Double, NumFeatures.type], label: Int) extends Data
}
