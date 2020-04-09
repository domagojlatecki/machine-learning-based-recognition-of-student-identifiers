package at.doml.thesis.nn

import scala.collection.immutable.ArraySeq
import scala.reflect.ClassTag

final class Vec[+A, S <: Int] private (val underlying: ArraySeq[A]) extends AnyVal {

  def length: Int = underlying.length

  def apply(i: Int): A = underlying(i)

  def map[B : ClassTag](f: A => B): Vec[B, S] = new Vec(underlying.map(f))

  def mapWithIndex[B : ClassTag](f: (A, Int) => B): Vec[B, S] =
    new Vec(ArraySeq.tabulate(underlying.length)(i => f(underlying(i), i)))
}

object Vec {

  def empty[A : ClassTag]: Vec[A, 0] = new Vec(ArraySeq.empty)

  def unsafeFromIterable[A : ClassTag, S <: Int](i: Iterable[A]): Vec[A, S] = new Vec(i.to(ArraySeq))

  def fill[A : ClassTag](n: Int)(elem: => A): Vec[A, n.type] = new Vec(ArraySeq.fill(n)(elem))

  def iterate[A : ClassTag](start: A, len: Int)(f: A => A): Vec[A, len.type] = new Vec(ArraySeq.iterate(start, len)(f))
}
