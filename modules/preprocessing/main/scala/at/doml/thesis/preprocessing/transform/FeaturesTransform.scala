package at.doml.thesis.preprocessing.transform

import at.doml.thesis.preprocessing.Data
import at.doml.thesis.preprocessing.debug.CanvasDebugger
import at.doml.thesis.preprocessing.image.{Canvas, Color}
import at.doml.thesis.util.collection.sized.Vec
import scala.collection.immutable.ArraySeq

object FeaturesTransform {

  def apply(
    canvas:     Canvas,
    canvasName: String
  )(implicit debugger: CanvasDebugger): Vec[Double, Data.NumFeatures.type] = {
    assert(canvas.width == canvas.height) // TODO make this type-safe somehow

    val size: Int = canvas.width
    val range = 0 until size

    def findLength(coords: Iterable[(Int, Int)]): Int = {
      coords.takeWhile(c => canvas.get(c._1, c._2) == Color.White).size
    }

    // top features
    val f1 = findLength(range.map(y => (size / 5, y)))
    val f2 = findLength(range.map(y => (2 * size / 5, y)))
    val f3 = findLength(range.map(y => (3 * size / 5, y)))
    val f4 = findLength(range.map(y => (4 * size / 5, y)))

    // bottom features
    val f5 = findLength(range.reverse.map(y => (size / 5, y)))
    val f6 = findLength(range.reverse.map(y => (2 * size / 5, y)))
    val f7 = findLength(range.reverse.map(y => (3 * size / 5, y)))
    val f8 = findLength(range.reverse.map(y => (4 * size / 5, y)))

    // left features
    val f9 = findLength(range.map(x => (x, size / 5)))
    val f10 = findLength(range.map(x => (x, 2 * size / 5)))
    val f11 = findLength(range.map(x => (x, 3 * size / 5)))
    val f12 = findLength(range.map(x => (x, 4 * size / 5)))

    // right features
    val f13 = findLength(range.reverse.map(x => (x, size / 5)))
    val f14 = findLength(range.reverse.map(x => (x, 2 * size / 5)))
    val f15 = findLength(range.reverse.map(x => (x, 3 * size / 5)))
    val f16 = findLength(range.reverse.map(x => (x, 4 * size / 5)))

    // middle height features
    val f17 = findLength((0 until size / 2).reverse.map(y => (size / 2, y)))
    val f18 = findLength((size / 2 until size).map(y => (size / 2, y)))
    // middle width features
    val f19 = findLength((0 until size / 2).reverse.map(x => (x, size / 2)))
    val f20 = findLength((size / 2 until size).map(x => (x, size / 2)))

    // diagonal features
    val f21 = findLength(range.zip(range))
    val f22 = findLength(range.reverse.zip(range))
    val f23 = findLength(range.zip(range.reverse))
    val f24 = findLength(range.reverse.zip(range.reverse))

    val heightFeatures = List(f1, f2, f3, f4, f5, f6, f7, f8).map(_ / size.toDouble)
    val widthFeatures = List(f9, f10, f11, f12, f13, f14, f15, f16).map(_ / size.toDouble)
    val middleFeatures = List(f17, f18, f19, f20).map(_ / size.toDouble * 2.0)
    val diagonalFeatures = List(f21, f22, f23, f24).map(_ / size.toDouble)

    debugger(
      {
        canvas.mapPixels { p =>
          if (
            // top features
            (p.x == size / 5 && p.y < f1) ||
            (p.x == 2 * size / 5 && p.y < f2) ||
            (p.x == 3 * size / 5 && p.y < f3) ||
            (p.x == 4 * size / 5 && p.y < f4) ||

            // bottom features
            (p.x == size / 5 && p.y + f5 >= size) ||
            (p.x == 2 * size / 5 && p.y + f6 >= size) ||
            (p.x == 3 * size / 5 && p.y + f7 >= size) ||
            (p.x == 4 * size / 5 && p.y + f8 >= size)
          ) {
            Color.Red
          } else if (
            // left features
            (p.y == size / 5 && p.x < f9) ||
            (p.y == 2 * size / 5 && p.x < f10) ||
            (p.y == 3 * size / 5 && p.x < f11) ||
            (p.y == 4 * size / 5 && p.x < f12) ||

            // bottom feature
            (p.y == size / 5 && p.x + f13 >= size) ||
            (p.y == 2 * size / 5 && p.x + f14 >= size) ||
            (p.y == 3 * size / 5 && p.x + f15 >= size) ||
            (p.y == 4 * size / 5 && p.x + f16 >= size)
          ) {
            Color.Blue
          } else if (
            // middle height features
            (p.x == size / 2 && size / 2 - p.y <= f17 && size / 2 - p.y >= 0) ||
            (p.x == size / 2 && p.y - size / 2 <= f18 && p.y - size / 2 >= 0) ||
            // middle width features
            (p.y == size / 2 && size / 2 - p.x <= f19 && size / 2 - p.x >= 0) ||
            (p.y == size / 2 && p.x - size / 2 <= f20 && p.x - size / 2 >= 0)
          ) {
            Color.Green
          } else if (
            // diagonal features
            (p.x == p.y && p.x < f21) ||
            (size - 1 - p.x == p.y && p.y < f22) ||
            (p.x == size - 1 - p.y && p.x < f23) ||
            (p.x == p.y && size - p.x <= f24)
          ) {
            Color.Purple
          } else {
            p.color
          }
        }
      },
      "features",
      canvasName
    )

    Vec.unsafeWrap((heightFeatures ::: widthFeatures ::: middleFeatures ::: diagonalFeatures).to(ArraySeq))
  }
}
