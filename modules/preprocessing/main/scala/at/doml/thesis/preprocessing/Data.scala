package at.doml.thesis.preprocessing

import at.doml.thesis.util.collection.sized.Vec

sealed trait Data extends Product with Serializable {

  val features: Vec[Double, Data.NumFeatures.type]

  val file: String

  val index: Int
}

object Data {

  val NumFeatures: Int = 24

  final case class Raw(features: Vec[Double, NumFeatures.type], file: String, index: Int) extends Data

  final case class Labeled(features: Vec[Double, NumFeatures.type], label: Int, file: String, index: Int) extends Data
}
