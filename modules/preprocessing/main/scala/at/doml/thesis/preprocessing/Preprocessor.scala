package at.doml.thesis.preprocessing

import at.doml.thesis.preprocessing.debug.CanvasDebugger
import at.doml.thesis.preprocessing.image.Canvas
import at.doml.thesis.preprocessing.transform._
import at.doml.thesis.util.Vec
import scala.collection.immutable.ArraySeq

object Preprocessor {

  def process(
    canvas:     Canvas,
    canvasName: String,
    n:          Int
  )(
    labels:   Option[Vec[Int, n.type]] = None,
    debugger: CanvasDebugger           = CanvasDebugger.NoOp
  ): Option[Vec[Data, n.type]] = {
    implicit val debug: CanvasDebugger = debugger

    val grayscale = GrayscaleTransform(canvas, canvasName)
    val contrast = ContrastTransform(grayscale, canvasName)
    val histogramGroups: Option[Vec[Canvas, n.type]] =
      if (n <= 1) {
        Some(Vec.unsafeWrap(ArraySeq(contrast)))
      } else {
        HistogramGroupingTransform(contrast, canvasName, n)
      }

    histogramGroups.map { groups =>
      val features = groups
        .mapWithIndex((c, i) => ResizeTransform(c, s"${canvasName}_${i.v}.png"))
        .mapWithIndex((c, i) => FeaturesTransform(c, s"${canvasName}_${i.v}.png"))

      labels match {
        case None     => features.map(Data.Raw)
        case Some(ls) => features.mapWith(ls)(Data.Labeled)
      }
    }
  }
}
