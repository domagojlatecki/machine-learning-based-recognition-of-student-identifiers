package at.doml.thesis.grad

sealed trait BatchSize extends Product with Serializable

object BatchSize {

  final case class of(v: Int) extends BatchSize

  case object all extends BatchSize
}
