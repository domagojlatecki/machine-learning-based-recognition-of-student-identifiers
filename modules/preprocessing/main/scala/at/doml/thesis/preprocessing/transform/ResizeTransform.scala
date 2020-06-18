package at.doml.thesis.preprocessing.transform

import at.doml.thesis.preprocessing.debug.CanvasDebugger
import at.doml.thesis.preprocessing.image.{Canvas, Color}
import scala.collection.View
import scala.collection.immutable.ArraySeq

object ResizeTransform {

  def apply(canvas: Canvas, canvasName: String)(implicit debugger: CanvasDebugger): Canvas = {
    val normalized = if (canvas.width > canvas.height) {
      val totalPadding = canvas.width - canvas.height
      val topPadding = totalPadding / 2
      val bottomPadding = totalPadding - topPadding
      val top = ArraySeq.fill(topPadding * canvas.width)(Color.White)
      val bottom = ArraySeq.fill(bottomPadding * canvas.width)(Color.White)

      Canvas(top ++ canvas.values ++ bottom, canvas.width, canvas.width)
    } else if (canvas.width < canvas.height) {
      val totalPadding = canvas.height - canvas.width
      val leftPadding = totalPadding / 2

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

    val padding = normalized.width / 5
    val newSize = normalized.width + 2 * padding
    val result = Canvas.blank(newSize, newSize).mapPixels { p =>
      if (p.x >= padding && p.x - padding < normalized.width && p.y >= padding && p.y - padding < normalized.height) {
        normalized.get(p.x - padding, p.y - padding)
      } else {
        p.color
      }
    }

    debugger(result, "resize", canvasName)

    result
  }
}
