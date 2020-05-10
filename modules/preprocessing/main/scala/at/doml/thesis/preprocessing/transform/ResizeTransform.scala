package at.doml.thesis.preprocessing.transform

import at.doml.thesis.preprocessing.debug.CanvasDebugger
import at.doml.thesis.preprocessing.image.{Canvas, Color}
import scala.collection.immutable.ArraySeq

object ResizeTransform {

  def apply(canvas: Canvas, canvasName: String)(implicit debugger: CanvasDebugger): Canvas = {
    val result = if (canvas.width > canvas.height) {
      val topSize = (canvas.width - canvas.height) / 2
      val bottomSize = (canvas.width - canvas.height) - topSize

      val topPadding = ArraySeq.fill(topSize * canvas.width)(Color.White)
      val bottomPadding = ArraySeq.fill(bottomSize * canvas.width)(Color.White)

      Canvas(topPadding ++ canvas.values ++ bottomPadding, canvas.width, canvas.width)
    } else if (canvas.width < canvas.height) {
      val leftPadding = (canvas.height - canvas.width) / 2

      Canvas.blank(canvas.height, canvas.height).mapPixels { p =>
        if (p.x >= leftPadding && p.x - leftPadding < canvas.width) {
          canvas.get(p.x - leftPadding, p.y)
        } else {
          p.color
        }
      }
    } else {
      canvas
    }

    debugger(result, "resize", canvasName)

    result
  }
}
