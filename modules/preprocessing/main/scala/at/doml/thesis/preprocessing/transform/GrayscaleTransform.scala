package at.doml.thesis.preprocessing.transform

import at.doml.thesis.preprocessing.debug.CanvasDebugger
import at.doml.thesis.preprocessing.image.{Canvas, Color}

object GrayscaleTransform {

  def apply(canvas: Canvas, canvasName: String)(implicit debugger: CanvasDebugger): Canvas = {
    val result = canvas.map { color =>
      val v = (Math.pow((color.red + color.green + color.blue) / 768.0, 2.0) * 256.0).toInt
      Color(color.alpha, v, v, v)
    }

    debugger(result, "grayscale", canvasName)
    result
  }
}
