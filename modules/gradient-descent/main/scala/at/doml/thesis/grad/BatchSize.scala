package at.doml.thesis.grad

sealed trait BatchSize

object BatchSize {

  final case class of(v: Int) extends BatchSize

  case object all extends BatchSize
}
