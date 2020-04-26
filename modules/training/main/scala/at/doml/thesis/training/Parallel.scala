package at.doml.thesis.training

import java.util.concurrent.Executors
import at.doml.thesis.util.Parallel
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}

object Parallel {

  object SingleThread extends Parallel {

    def itemsPerThread: Int = Int.MaxValue

    def execute[A](tasks: Iterator[() => A]): List[A] = tasks.map(_.apply()).toList

    def shutdown(): Unit = ()
  }

  class NumProcessors extends Parallel {

    val itemsPerThread: Int = 100

    private val tp = Executors.newFixedThreadPool(Runtime.getRuntime.availableProcessors())
    private implicit val ec: ExecutionContext =
      ExecutionContext.fromExecutorService(tp)

    def execute[A](tasks: Iterator[() => A]): List[A] = {
      tasks.map(f => Future(f()))
        .toList
        .map(f => Await.result(f, Duration.Inf))
    }

    def shutdown(): Unit = tp.shutdown()
  }
}
