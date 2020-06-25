package at.doml.thesis.recognition

import java.awt.image.BufferedImage
import java.nio.file.Path
import at.doml.thesis.preprocessing.debug.CanvasDebugger
import at.doml.thesis.preprocessing.image.Canvas
import javax.imageio.ImageIO

final class FileCanvasDebugger(debugRoot: Path) extends CanvasDebugger {

  def apply(canvas: => Canvas, debugStepName: String, canvasName: String): Unit = {
    val debugDir = debugRoot.resolve(s"debug-$debugStepName")

    if (debugDir.toFile.mkdir()) {
      println(s"Creating debug directory: $debugDir")
    }

    val outPath = debugDir.resolve(s"$canvasName.png")

    writeCanvas(canvas, outPath)
  }

  private def writeCanvas(canvas: Canvas, path: Path): Unit = {
    val out = new BufferedImage(canvas.width, canvas.height, BufferedImage.TYPE_INT_ARGB)
    out.setRGB(0, 0, canvas.width, canvas.height, canvas.values.toArray.map(_.value), 0, canvas.width)
    ImageIO.write(out, "PNG", path.toFile)
  }
}
