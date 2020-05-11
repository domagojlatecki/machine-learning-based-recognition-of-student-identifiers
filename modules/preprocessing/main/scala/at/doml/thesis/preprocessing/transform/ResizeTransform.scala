package at.doml.thesis.preprocessing.transform

import at.doml.thesis.preprocessing.debug.CanvasDebugger
import at.doml.thesis.preprocessing.image.{Canvas, Color}
import scala.collection.View
import scala.collection.immutable.ArraySeq

object ResizeTransform {

  private implicit final class ViewOps[A](view: View[A]) {

    def avgBy(f: A => Int): Int = {
      val (sum, count) = view.foldLeft((0, 0)){ case ((s, c), a) =>
        (s + f(a), c + 1)
      }

      sum / count
    }
  }

  def apply(canvas: Canvas, canvasName: String)(implicit debugger: CanvasDebugger): Canvas = {
    val result = if (canvas.width > canvas.height) {
      val currentCenterY = canvas.getAllPixelsWithColor(Color.Black).avgBy(_.y)
      val targetCenterY = canvas.height / 2
      val yDiff = targetCenterY - currentCenterY
      val topSize = ((canvas.width - canvas.height) / 2 + yDiff) min (canvas.width - canvas.height)
      val bottomSize = ((canvas.width - canvas.height) - topSize) max 0

      val topPadding = ArraySeq.fill(topSize * canvas.width)(Color.White)
      val bottomPadding = ArraySeq.fill(bottomSize * canvas.width)(Color.White)

      Canvas(topPadding ++ canvas.values ++ bottomPadding, canvas.width, canvas.width)
    } else if (canvas.width < canvas.height) {
      val currentCenterX = canvas.getAllPixelsWithColor(Color.Black).avgBy(_.x)
      val targetCenterX = canvas.width / 2
      val xDiff = targetCenterX - currentCenterX
      val leftPadding = ((canvas.height - canvas.width) / 2 + xDiff) min (canvas.height - canvas.width)

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
