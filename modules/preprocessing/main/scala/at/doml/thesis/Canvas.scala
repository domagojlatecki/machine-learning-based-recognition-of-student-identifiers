package at.doml.thesis

import java.awt.image.BufferedImage
import java.nio.file.Path
import javax.imageio.ImageIO
import scala.collection.immutable.ArraySeq

final class Canvas private (p: ArraySeq[RgbColor], w: Int, h: Int) {

  def writeTo(path: Path): Unit = {
    val out = new BufferedImage(w, h, BufferedImage.TYPE_INT_ARGB)
    out.setRGB(0, 0, w, h, p.toArray.map(_.toInt), 0, w)
    ImageIO.write(out, "PNG", path.toFile)
  }

  def map(f: Pixel => RgbColor): Canvas = {
    val np = p.zipWithIndex.map { case (c, i) =>
      val x = i % w
      val y = i / w
      f(Pixel(c, x, y))
    }

    new Canvas(np, w, h)
  }
}

object Canvas {

  def fromPath(path: Path): Canvas = {
    val image = ImageIO.read(path.toFile)
    val width = image.getWidth
    val height = image.getHeight
    val pixels = image.getRGB(0, 0, width, height, null: Array[Int], 0, width).map(RgbColor(_))

    new Canvas(ArraySeq.unsafeWrapArray(pixels), width, height)
  }
}
