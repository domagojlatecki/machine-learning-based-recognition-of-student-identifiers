package at.doml.thesis.preprocessing.transform

import at.doml.thesis.preprocessing.debug.CanvasDebugger
import at.doml.thesis.preprocessing.image.{Canvas, Color}

object ContrastTransform {

  private def intensity(c: Color): Int = c.red + c.green + c.blue

  def apply(canvas: Canvas)(implicit debugger: CanvasDebugger): Canvas = {
    val intensities = canvas.values.filter(_.alpha == 255).map(intensity)
    val minIntensity = intensities.min
    val maxIntensity = intensities.max
    val cutoffPoint = (maxIntensity - minIntensity) / 2.0

    val result = canvas.map { color =>
      if (color.alpha < 255) {
        Color.White
      } else if (intensity(color) > cutoffPoint) {
        Color.White
      } else {
        Color.Black
      }
    }

    debugger(result, "contrast", 0)
    result
  }
}
