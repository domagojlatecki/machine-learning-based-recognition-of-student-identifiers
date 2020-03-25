package at.doml.thesis

import java.awt.image.BufferedImage
import java.nio.file.Path
import javax.imageio.ImageIO
import scala.collection.immutable.ArraySeq

// TODO refactor
final class Canvas private (p: ArraySeq[Color], w: Int, h: Int) {

  val linear: ArraySeq[Color] = p
  val area: Int = w * h

  def writeTo(path: Path): Unit = {
    val out = new BufferedImage(w, h, BufferedImage.TYPE_INT_ARGB)
    out.setRGB(0, 0, w, h, p.toArray.map(_.toInt), 0, w)
    ImageIO.write(out, "PNG", path.toFile)
  }

  def mapPixels(f: Pixel => Color): Canvas = {
    val np = p.zipWithIndex.map { case (c, i) =>
      val x = i % w
      val y = i / w
      f(Pixel(c, x, y))
    }

    new Canvas(np, w, h)
  }

  def map(f: Color => Color): Canvas = {
    new Canvas(p.map(f), w, h)
  }

  def mapGrouped(xn: Int, yn: Int)(f: ArraySeq[Color] => Color): Canvas = {
    val area = for {
      yRange <- 0 until h grouped yn
      xRange <- 0 until w grouped xn
    } yield for {
      x <- xRange.to(ArraySeq)
      y <- yRange
    } yield {
      p(y * w + x)
    }

    new Canvas(area.map(f).to(ArraySeq), (w.toDouble / xn).ceil.toInt, (h.toDouble / yn).ceil.toInt)
  }
}

object Canvas {

  def fromPath(path: Path): Canvas = {
    val image = ImageIO.read(path.toFile)
    val width = image.getWidth
    val height = image.getHeight
    val pixels = image.getRGB(0, 0, width, height, null: Array[Int], 0, width).map(Color(_))

    new Canvas(ArraySeq.unsafeWrapArray(pixels), width, height)
  }
}
