package at.doml.thesis.preprocessing

import at.doml.thesis.preprocessing.debug.CanvasDebugger
import at.doml.thesis.preprocessing.image.{Canvas, Color, Point}
import at.doml.thesis.preprocessing.transform.{ContrastTransform, GrayscaleTransform, GroupingTransform, HotspotsTransform}
import at.doml.thesis.util.Vec

object Preprocessor {

  // TODO improve this
  private def findLeftAndRightEdges(canvas: Canvas): (Int, Int) = {
    val trim = canvas.pixels
      .groupBy(_.x)
      .toList
      .sortBy(_._1)
      .map { case (_, row) =>
        row.map(_.color).reduce((a, b) => if (a == Color.Black || b == Color.Black) Color.Black else Color.White)
      }

    val left = trim.takeWhile(_ == Color.White).size
    val right = trim.reverse.takeWhile(_ == Color.White).size

    (left, right)
  }

  private def createCentroids(n: Int, left: Int, right: Int, width: Int, height: Int): Vec[Point, n.type] =
    Vec.tabulate(n) { i =>
      Point(
        left + (width - left - right) / 10.0 * ((2 * i + 1) / 2.0),
        height / 2.0
      )
    }

  def process(
    canvas:     Canvas,
    canvasName: String,
    n:          Int
  )(
    labels:   Option[Vec[Int, n.type]] = None,
    debugger: CanvasDebugger           = CanvasDebugger.NoOp
  ): Vec[Data, n.type] = {
    implicit val debug: CanvasDebugger = debugger

    val grayscale = GrayscaleTransform(canvas, canvasName)
    val contrast = ContrastTransform(grayscale, canvasName)
    val (left, right) = findLeftAndRightEdges(contrast)
    val initCentroids = createCentroids(n, left, right, contrast.width, contrast.height)
    val blackPixels = contrast.getAllPointsWithColor(Color.Black)
    val centroids = KMeansGrouping.findCentroids(initCentroids, blackPixels)
    val groups = GroupingTransform(contrast, canvasName, n)(centroids)
    val hotspots = groups.mapWithIndex { (group, i) => HotspotsTransform(group, s"$canvasName-${i.v}")}

    labels match {
      case None     => hotspots.map(Data.Raw)
      case Some(ls) => hotspots.mapWith(ls)(Data.Labeled)
    }
  }
}
