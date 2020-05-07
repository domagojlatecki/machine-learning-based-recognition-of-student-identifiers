package at.doml.thesis.preprocessing.transform

import at.doml.thesis.preprocessing.Data
import at.doml.thesis.preprocessing.debug.CanvasDebugger
import at.doml.thesis.preprocessing.image.{Canvas, Color}
import at.doml.thesis.util.Vec
import scala.collection.immutable.ArraySeq

object FeaturesTransform {

  def apply(
    canvas:     Canvas,
    canvasName: String
  )(implicit debugger: CanvasDebugger): Vec[Double, Data.NumFeatures.type] = {
    val wRange = 0 until canvas.width
    val hRange = 0 until canvas.height

    def findLength(coords: Iterable[(Int, Int)]): Int = {
      coords.takeWhile(c => canvas.get(c._1, c._2)== Color.White).size
    }

    // top features
    val f1 = findLength(hRange.map(y => (canvas.width / 5, y)))
    val f2 = findLength(hRange.map(y => (2 * canvas.width / 5, y)))
    val f3 = findLength(hRange.map(y => (3 * canvas.width / 5, y)))
    val f4 = findLength(hRange.map(y => (4 * canvas.width / 5, y)))

    // bottom features
    val f5 = findLength(hRange.reverse.map(y => (canvas.width / 5, y)))
    val f6 = findLength(hRange.reverse.map(y => (2 * canvas.width / 5, y)))
    val f7 = findLength(hRange.reverse.map(y => (3 * canvas.width / 5, y)))
    val f8 = findLength(hRange.reverse.map(y => (4 * canvas.width / 5, y)))

    // left features
    val f9 = findLength(wRange.map(x => (x, canvas.height / 5)))
    val f10 = findLength(wRange.map(x => (x, 2 * canvas.height / 5)))
    val f11 = findLength(wRange.map(x => (x, 3 * canvas.height / 5)))
    val f12 = findLength(wRange.map(x => (x, 4 * canvas.height / 5)))

    // right features
    val f13 = findLength(wRange.reverse.map(x => (x, canvas.height / 5)))
    val f14 = findLength(wRange.reverse.map(x => (x, 2 * canvas.height / 5)))
    val f15 = findLength(wRange.reverse.map(x => (x, 3 * canvas.height / 5)))
    val f16 = findLength(wRange.reverse.map(x => (x, 4 * canvas.height / 5)))

    // middle height features
    val f17 = findLength((0 until canvas.height / 2).reverse.map(y => (canvas.width / 2, y)))
    val f18 = findLength((canvas.height / 2 until canvas.height).map(y => (canvas.width / 2, y)))
    // middle width features
    val f19 = findLength((0 until canvas.width / 2).reverse.map(x => (x, canvas.height / 2)))
    val f20 = findLength((canvas.width / 2 until canvas.width).map(x => (x, canvas.height / 2)))

    val heightFeatures = List(f1, f2, f3, f4, f5, f6, f7, f8).map(_ / canvas.height.toDouble)
    val widthFeatures = List(f9, f10, f11, f12, f13, f14, f15, f16).map(_ / canvas.width.toDouble)
    val middleHeightFeatures = List(f17, f18).map(_ / canvas.height.toDouble * 2.0)
    val middleWidthFeatures = List(f19, f20).map(_ / canvas.width.toDouble * 2.0)

    debugger(
      {
        canvas.mapPixels { p =>
          if (
            // top features
            (p.x == canvas.width / 5 && p.y < f1) ||
            (p.x == 2 * canvas.width / 5 && p.y < f2) ||
            (p.x == 3 * canvas.width / 5 && p.y < f3) ||
            (p.x == 4 * canvas.width / 5 && p.y < f4) ||

            // bottom features
            (p.x == canvas.width / 5 && p.y + f5 >= canvas.height) ||
            (p.x == 2 * canvas.width / 5 && p.y + f6 >= canvas.height) ||
            (p.x == 3 * canvas.width / 5 && p.y + f7 >= canvas.height) ||
            (p.x == 4 * canvas.width / 5 && p.y + f8 >= canvas.height)
          ) {
            Color.Red
          } else if (
            // left features
            (p.y == canvas.height / 5 && p.x < f9) ||
            (p.y == 2 * canvas.height / 5 && p.x < f10) ||
            (p.y == 3 * canvas.height / 5 && p.x < f11) ||
            (p.y == 4 * canvas.height / 5 && p.x < f12) ||

            // bottom feature
            (p.y == canvas.height / 5 && p.x + f13 >= canvas.width) ||
            (p.y == 2 * canvas.height / 5 && p.x + f14 >= canvas.width) ||
            (p.y == 3 * canvas.height / 5 && p.x + f15 >= canvas.width) ||
            (p.y == 4 * canvas.height / 5 && p.x + f16 >= canvas.width)
          ) {
            Color.Blue
          } else if (
            // middle height features
            (p.x == canvas.width / 2 && canvas.height / 2 - p.y <= f17 && canvas.height / 2 - p.y >= 0) ||
            (p.x == canvas.width / 2 && p.y - canvas.height / 2 <= f18 && p.y - canvas.height / 2 >= 0) ||
            // middle width features
            (p.y == canvas.height / 2 && canvas.width / 2 - p.x <= f19 && canvas.width / 2 - p.x >= 0) ||
            (p.y == canvas.height / 2 && p.x - canvas.width / 2 <= f20 && p.x - canvas.width / 2 >= 0)
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

    Vec.unsafeWrap((heightFeatures ::: widthFeatures ::: middleHeightFeatures ::: middleWidthFeatures).to(ArraySeq))
  }
}
