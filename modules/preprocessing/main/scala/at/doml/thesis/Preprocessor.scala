package at.doml.thesis

import java.nio.file.Paths

object Preprocessor {

  def grayscale(c: RgbColor): RgbColor = {
    val v = (Math.pow((c.r + c.g + c.b) / 768.0, 2.0) * 256.0).toInt
    RgbColor(v & 0xFF, v & 0xFF, v & 0xFF)
  }

  def main(args: Array[String]): Unit = {
    Canvas.fromPath(Paths.get("in.png"))
      .map(p => grayscale(p.color))
      .writeTo(Paths.get("out.png"))
  }
}
