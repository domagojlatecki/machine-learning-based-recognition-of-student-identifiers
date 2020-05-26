package at.doml.thesis.util.collection.sized

import at.doml.thesis.util.par.Parallel
import scala.collection.immutable.ArraySeq
import scala.reflect.ClassTag

final class Vec[+A, S <: Int] private[util] (val underlying: ArraySeq[A]) extends AnyVal {

  def apply(i: Idx[S]): A = underlying(i.v)

  def length: S = underlying.length.asInstanceOf[S]

  def indices: Indices[S] = Indices.until(length).asInstanceOf[Indices[S]]

  def map[B : ClassTag](f: A => B): Vec[B, S] = new Vec(underlying.map(f))

  def parMap[B : ClassTag](f: A => B)(implicit par: Parallel): Vec[B, S] = {
    val out = new Array[B](length)
    val tasks = underlying.indices.grouped(par.itemsPerThread).map { indices =>
      () => {
        for (i <- indices) {
          out(i) = f(underlying(i))
        }
      }
    }

    par.execute(tasks)

    new Vec(ArraySeq.unsafeWrapArray(out))
  }

  def mapWith[B, C : ClassTag](that: Vec[B, S])(f: (A, B) => C): Vec[C, S] =
    new Vec(ArraySeq.tabulate(length)(i => f(underlying(i), that.underlying(i))))

  def mapWith[B, C, D : ClassTag](that1: Vec[B, S], that2: Vec[C, S])(f: (A, B, C) => D): Vec[D, S] =
    new Vec(ArraySeq.tabulate(length)(i => f(underlying(i), that1.underlying(i), that2.underlying(i))))

  def parMapWith[B, C : ClassTag](that: Vec[B, S])(f: (A, B) => C)(implicit par: Parallel): Vec[C, S] = {
    val out = new Array[C](length)
    val tasks = underlying.indices.grouped(par.itemsPerThread).map { indices =>
      () => {
        for (i <- indices) {
          out(i) = f(underlying(i), that.underlying(i))
        }
      }
    }

    par.execute(tasks)

    new Vec(ArraySeq.unsafeWrapArray(out))
  }

  def mapWithIndex[B : ClassTag](f: (A, Idx[S]) => B): Vec[B, S] =
    new Vec(ArraySeq.tabulate(length)(i => f(underlying(i), Idx(i))))

  def parMapWithIndex[B : ClassTag](f: (A, Idx[S]) => B)(implicit par: Parallel): Vec[B, S] = {
    val out = new Array[B](length)
    val tasks = underlying.indices.grouped(par.itemsPerThread).map { indices =>
      () => {
        for (i <- indices) {
          out(i) = f(underlying(i), Idx(i))
        }
      }
    }

    par.execute(tasks)

    new Vec(ArraySeq.unsafeWrapArray(out))
  }

  def minBy[B](f: A => B)(implicit cmp: Ordering[B]): A =
    underlying.minBy(f)

  def maxBy[B](f: A => B)(implicit cmp: Ordering[B]): A =
    underlying.maxBy(f)
}

object Vec {

  def empty[A : ClassTag]: Vec[A, 0] = new Vec(ArraySeq.empty)

  def unsafeWrap[A, S <: Int](a: ArraySeq[A]): Vec[A, S] = new Vec(a)

  def fill[A : ClassTag](n: Int)(elem: => A): Vec[A, n.type] = new Vec(ArraySeq.fill(n)(elem))

  def tabulate[A : ClassTag](n: Int)(f: Int => A): Vec[A, n.type] = new Vec(ArraySeq.tabulate(n)(f))
}
