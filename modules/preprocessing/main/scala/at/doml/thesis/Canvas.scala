package at.doml.thesis

import java.awt.image.BufferedImage
import java.nio.file.Path
import javax.imageio.ImageIO
import scala.collection.immutable.ArraySeq

// TODO refactor
final class Canvas private (p: ArraySeq[Color], w: Int, h: Int) {

  val linear: ArraySeq[Color] = p
  def pixels: ArraySeq[Pixel] = {
    p.zipWithIndex.map { case (c, i) =>
      val x = i % w
      val y = i / w
      Pixel(c, x, y)
    }
  }
  val area: Int = w * h
  val width: Int = w

  def writeTo(path: Path): Unit = {
    val out = new BufferedImage(w, h, BufferedImage.TYPE_INT_ARGB)
    out.setRGB(0, 0, w, h, p.toArray.map(_.toInt), 0, w)
    ImageIO.write(out, "PNG", path.toFile)
  }

  def mapPixels(f: Pixel => Color): Canvas = {
    new Canvas(pixels.map(f), w, h)
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

  def fromColumns(xMin: Int, xMax: Int): Canvas = {
    val columns = for {
      x <- xMin until xMax
    } yield for {
      y <- (0 until h).to(ArraySeq)
    } yield {
      p(y * w + x)
    }

    val out = for {
      y <- 0 until h
      x <- 0 until (xMax - xMin)
    } yield {
      columns(x)(y)
    }

    new Canvas(out.to(ArraySeq), xMax - xMin, h)
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
