package at.doml.thesis.util

final class Indices[S <: Int] private (val underlying: Range) extends AnyVal {

  def foreach[@specialized(Unit) U](f: Idx[S] => U): Unit = underlying.foreach(i => f(Idx(i)))

  def end: S = underlying.end.asInstanceOf[S]
}

object Indices {

  def until(end: Int): Indices[end.type] = new Indices(0 until end)
}
