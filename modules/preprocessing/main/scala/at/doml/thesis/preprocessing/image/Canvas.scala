package at.doml.thesis.preprocessing.image

import scala.collection.View
import scala.collection.immutable.ArraySeq

final case class Canvas(values: ArraySeq[Color], width: Int, height: Int) {

  def get(x: Int, y: Int): Color =
    values(x + y * width)

  def pixels: View[Pixel] =
    values.view.zipWithIndex.map { case (c, i) =>
      val x = i % width
      val y = i / width
      Pixel(c, x, y)
    }

  def map(f: Color => Color): Canvas =
    Canvas(values.map(f), width, height)

  def mapPixels(f: Pixel => Color): Canvas =
    Canvas(pixels.map(f).to(ArraySeq), width, height)

  def getAllPointsWithColor(color: Color): ArraySeq[Point] =
    pixels.filter(_.color == color).map(p => Point(p.x, p.y)).to(ArraySeq)
}

object Canvas {

  def blank(width: Int, height: Int): Canvas = {
    new Canvas(ArraySeq.fill(width * height)(Color.White), width, height)
  }
}
