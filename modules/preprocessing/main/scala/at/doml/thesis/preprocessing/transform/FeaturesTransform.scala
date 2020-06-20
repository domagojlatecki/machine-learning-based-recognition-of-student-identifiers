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
    val f1 = findLength(range.map(y => (size / 7, y)))
    val f2 = findLength(range.map(y => (2 * size / 7, y)))
    val f3 = findLength(range.map(y => (3 * size / 7, y)))
    val f4 = findLength(range.map(y => (4 * size / 7, y)))
    val f5 = findLength(range.map(y => (5 * size / 7, y)))
    val f6 = findLength(range.map(y => (6 * size / 7, y)))

    // bottom features
    val f7 = findLength(range.reverse.map(y => (size / 7, y)))
    val f8 = findLength(range.reverse.map(y => (2 * size / 7, y)))
    val f9 = findLength(range.reverse.map(y => (3 * size / 7, y)))
    val f10 = findLength(range.reverse.map(y => (4 * size / 7, y)))
    val f11 = findLength(range.reverse.map(y => (5 * size / 7, y)))
    val f12 = findLength(range.reverse.map(y => (6 * size / 7, y)))

    // left features
    val f13 = findLength(range.map(x => (x, size / 7)))
    val f14 = findLength(range.map(x => (x, 2 * size / 7)))
    val f15 = findLength(range.map(x => (x, 3 * size / 7)))
    val f16 = findLength(range.map(x => (x, 4 * size / 7)))
    val f17 = findLength(range.map(x => (x, 5 * size / 7)))
    val f18 = findLength(range.map(x => (x, 6 * size / 7)))

    // right features
    val f19 = findLength(range.reverse.map(x => (x, size / 7)))
    val f20 = findLength(range.reverse.map(x => (x, 2 * size / 7)))
    val f21 = findLength(range.reverse.map(x => (x, 3 * size / 7)))
    val f22 = findLength(range.reverse.map(x => (x, 4 * size / 7)))
    val f23 = findLength(range.reverse.map(x => (x, 5 * size / 7)))
    val f24 = findLength(range.reverse.map(x => (x, 6 * size / 7)))

    val middleToLeft = (0 until size / 2).reverse
    val middleToUp = middleToLeft
    val middleToRight = size / 2 until size
    val middleToDown = middleToRight
    // middle height features
    val f25 = findLength(middleToLeft.map(y => (size / 2, y)))
    val f26 = findLength(middleToRight.map(y => (size / 2, y)))
    // middle width features
    val f27 = findLength(middleToUp.map(x => (x, size / 2)))
    val f28 = findLength(middleToDown.map(x => (x, size / 2)))
    // middle diagonal features
    val f29 = findLength(middleToLeft.zip(middleToUp))
    val f30 = findLength(middleToRight.zip(middleToUp))
    val f31 = findLength(middleToLeft.zip(middleToDown))
    val f32 = findLength(middleToRight.zip(middleToDown))

    // diagonal corner features
    val f33 = findLength(range.zip(range))
    val f34 = findLength(range.reverse.zip(range))
    val f35 = findLength(range.zip(range.reverse))
    val f36 = findLength(range.reverse.zip(range.reverse))

    val heightFeatures = List(f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12).map(_ / size.toDouble)
    val widthFeatures = List(f13, f14, f15, f16, f17, f18, f19, f20, f21, f22, f23, f24).map(_ / size.toDouble)
    val middleFeatures = List(f25, f26, f27, f28, f29, f30, f31, f32).map(_ / size.toDouble * 2.0)
    val diagonalFeatures = List(f33, f34, f35, f36).map(_ / size.toDouble)

    debugger(
      {
        canvas.mapPixels { p =>
           if (
            // diagonal corner features
            (p.x == p.y && p.x < f33) ||
            (size - 1 - p.x == p.y && p.y < f34) ||
            (p.x == size - 1 - p.y && p.x < f35) ||
            (p.x == p.y && size - p.x <= f36)
          ) {
            Color.Purple
          } else if (
            // top features
            (p.x == size / 7 && p.y < f1) ||
            (p.x == 2 * size / 7 && p.y < f2) ||
            (p.x == 3 * size / 7 && p.y < f3) ||
            (p.x == 4 * size / 7 && p.y < f4) ||
            (p.x == 5 * size / 7 && p.y < f5) ||
            (p.x == 6 * size / 7 && p.y < f6) ||

            // bottom features
            (p.x == size / 7 && p.y + f7 >= size) ||
            (p.x == 2 * size / 7 && p.y + f8 >= size) ||
            (p.x == 3 * size / 7 && p.y + f9 >= size) ||
            (p.x == 4 * size / 7 && p.y + f10 >= size) ||
            (p.x == 5 * size / 7 && p.y + f11 >= size) ||
            (p.x == 6 * size / 7 && p.y + f12 >= size)
          ) {
            Color.Red
          } else if (
            // left features
            (p.y == size / 7 && p.x < f13) ||
            (p.y == 2 * size / 7 && p.x < f14) ||
            (p.y == 3 * size / 7 && p.x < f15) ||
            (p.y == 4 * size / 7 && p.x < f16) ||
            (p.y == 5 * size / 7 && p.x < f17) ||
            (p.y == 6 * size / 7 && p.x < f18) ||

            // bottom features
            (p.y == size / 7 && p.x + f19 >= size) ||
            (p.y == 2 * size / 7 && p.x + f20 >= size) ||
            (p.y == 3 * size / 7 && p.x + f21 >= size) ||
            (p.y == 4 * size / 7 && p.x + f22 >= size) ||
            (p.y == 5 * size / 7 && p.x + f23 >= size) ||
            (p.y == 6 * size / 7 && p.x + f24 >= size)
          ) {
            Color.Blue
          } else if (
            // middle height features
            (p.x == size / 2 && size / 2 - p.y <= f25 && size / 2 - p.y >= 0) ||
            (p.x == size / 2 && p.y - size / 2 <= f26 && p.y - size / 2 >= 0) ||

            // middle width features
            (p.y == size / 2 && size / 2 - p.x <= f27 && size / 2 - p.x >= 0) ||
            (p.y == size / 2 && p.x - size / 2 <= f28 && p.x - size / 2 >= 0) ||

            // middle diagonal features
            (p.x == p.y && size / 2 - p.x <= f29 && size / 2 - p.x >= 0) ||
            (size - 1 - p.x == p.y && size / 2 - p.y <= f30 && size / 2 - p.y >= 0) ||
            (p.x == size - 1 - p.y && p.y - size / 2 <= f31 && p.y - size / 2 >= 0) ||
            (p.x == p.y && p.x - size / 2 <= f32 && p.x - size / 2 >= 0)
          ) {
            Color.Green
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
