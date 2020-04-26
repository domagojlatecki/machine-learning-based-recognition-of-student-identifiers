package at.doml.thesis.training

import java.awt.image.BufferedImage
import java.nio.file.Path
import at.doml.thesis.preprocessing.debug.CanvasDebugger
import at.doml.thesis.preprocessing.image.Canvas
import javax.imageio.ImageIO

object FileCanvasDebugger extends CanvasDebugger {

  def apply(canvas: => Canvas, debugStepName: String, canvasIndex: Int): Unit = {
    // TODO implement debugger
  }

  private def writeCanvas(canvas: Canvas, path: Path): Unit = {
    val out = new BufferedImage(canvas.width, canvas.height, BufferedImage.TYPE_INT_ARGB)
    out.setRGB(0, 0, canvas.width, canvas.height, canvas.values.toArray.map(_.value), 0, canvas.width)
    ImageIO.write(out, "PNG", path.toFile)
  }
}
