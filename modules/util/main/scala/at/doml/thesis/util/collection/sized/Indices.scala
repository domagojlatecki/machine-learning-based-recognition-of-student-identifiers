package at.doml.thesis.util.collection.sized

import scala.collection.immutable.ArraySeq
import scala.reflect.ClassTag

final class Indices[S <: Int] private (val underlying: Range) extends AnyVal {

  def foreach[@specialized(Unit) U](f: Idx[S] => U): Unit = underlying.foreach(i => f(Idx(i)))

  def end: S = underlying.end.asInstanceOf[S]

  def map[B : ClassTag](f: Idx[S] => B): Vec[B, S] = new Vec(underlying.map(i => f(Idx(i))).to(ArraySeq))

  def grouped(size: Int): Iterator[Indices[S]] = underlying.grouped(size).map(new Indices(_))
}

object Indices {

  def until(end: Int): Indices[end.type] = new Indices(0 until end)
}
