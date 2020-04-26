package at.doml.thesis.util

trait Parallel {

  def itemsPerThread: Int

  def execute[A](tasks: Iterator[() => A]): List[A]

  def shutdown(): Unit
}
