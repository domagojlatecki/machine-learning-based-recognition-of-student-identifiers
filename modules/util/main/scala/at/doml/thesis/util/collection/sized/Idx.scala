package at.doml.thesis.util.collection.sized

final class Idx[A <: Int] private (val v: Int) extends AnyVal

private[util] object Idx {
  def apply[A <: Int](v: Int): Idx[A] = new Idx(v)
}
