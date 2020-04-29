package at.doml.thesis.preprocessing.debug

import at.doml.thesis.preprocessing.image.Canvas

trait CanvasDebugger {
  def apply(canvas: => Canvas, debugStepName: String, canvasName: String): Unit
}

object CanvasDebugger {
  object NoOp extends CanvasDebugger {
    def apply(canvas: => Canvas, debugStepName: String, canvasName: String): Unit = ()
  }
}
