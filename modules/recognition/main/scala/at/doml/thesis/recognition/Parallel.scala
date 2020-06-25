package at.doml.thesis.recognition

import java.util.concurrent.Executors
import at.doml.thesis.util.par.Parallel
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration.Duration

object Parallel {

  val DefaultItemsPerThread: Int = 100

  final class NumProcessors extends Parallel {

    val itemsPerThread: Int = DefaultItemsPerThread

    private val tp = Executors.newFixedThreadPool(Runtime.getRuntime.availableProcessors())
    private implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(tp)

    def execute[A](tasks: Iterator[() => A]): List[A] = {
      tasks.map(f => Future(f()))
        .toList
        .map(f => Await.result(f, Duration.Inf))
    }

    def shutdown(): Unit = tp.shutdown()
  }

  object SingleThread extends Parallel {

    val itemsPerThread: Int = Int.MaxValue

    def execute[A](tasks: Iterator[() => A]): List[A] =
      tasks.map(_()).toList

    def shutdown(): Unit = ()
  }
}
