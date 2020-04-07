package at.doml.thesis.nn

import scala.collection.immutable.ArraySeq
import scala.reflect.ClassTag

final class Vec[+A, S <: Int] private (val underlying: ArraySeq[A]) extends AnyVal {

  def length: Int = underlying.length

  def map[B](f: A => B): Vec[B, S] = new Vec(underlying.map(f))
}

object Vec {

  def empty[A : ClassTag]: Vec[A, 0] = new Vec(ArraySeq.empty)

  def fill[A : ClassTag](n: Int)(elem: => A): Vec[A, n.type] = new Vec(ArraySeq.fill(n)(elem))
}
