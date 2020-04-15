package at.doml.thesis.util

import scala.collection.immutable.ArraySeq
import scala.reflect.ClassTag

final class Vec[+A, S <: Int] private[util] (val underlying: ArraySeq[A]) extends AnyVal {

  def apply(i: Idx[S]): A = underlying(i.v)

  def length: S = underlying.length.asInstanceOf[S]

  def indices: Indices[S] = Indices.until(length).asInstanceOf[Indices[S]]

  def map[B : ClassTag](f: A => B): Vec[B, S] = new Vec(underlying.map(f))

  def mapWith[B, C : ClassTag](that: Vec[B, S])(f: (A, B) => C): Vec[C, S] =
    new Vec(ArraySeq.tabulate(length)(i => f(underlying(i), that.underlying(i))))

  def mapWithIndex[B : ClassTag](f: (A, Idx[S]) => B): Vec[B, S] =
    new Vec(ArraySeq.tabulate(length)(i => f(underlying(i), Idx(i))))

  @deprecated
  def unsafeMapWithIndex[B : ClassTag](f: (A, Int) => B): Vec[B, S] =
    new Vec(ArraySeq.tabulate(length)(i => f(underlying(i), i)))
}

object Vec {

  def unsafeWrap[A, S <: Int](a: ArraySeq[A]): Vec[A, S] = new Vec(a)

  def fill[A : ClassTag](n: Int)(elem: => A): Vec[A, n.type] = new Vec(ArraySeq.fill(n)(elem))

  @deprecated
  def unsafeFromIterable[A : ClassTag, S <: Int](i: Iterable[A]): Vec[A, S] = new Vec(i.to(ArraySeq))

  @deprecated
  def iterate[A : ClassTag](start: A, len: Int)(f: A => A): Vec[A, len.type] = new Vec(ArraySeq.iterate(start, len)(f))
}
