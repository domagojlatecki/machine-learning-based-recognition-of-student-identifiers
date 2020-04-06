package at.doml.thesis.nn

import scala.collection.immutable.ArraySeq
import scala.reflect.ClassTag
import Vec.Size

final class Vec[+A, +S <: Size] private (val underlying: ArraySeq[A]) extends AnyVal {

  def length: Int = underlying.length

  def map[B](f: A => B): Vec[B, S] = new Vec(underlying.map(f))
}

object Vec {

  type Size = Int with Singleton

  def empty[A : ClassTag]: Vec[A, 0] = new Vec(ArraySeq.empty)

  def fill[A : ClassTag, S <: Size](n: S)(elem: => A): Vec[A, S] = new Vec(ArraySeq.fill(n)(elem))
}
