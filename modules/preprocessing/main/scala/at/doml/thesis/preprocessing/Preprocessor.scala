package at.doml.thesis.preprocessing

import at.doml.thesis.preprocessing.debug.CanvasDebugger
import at.doml.thesis.preprocessing.image.Canvas
import at.doml.thesis.preprocessing.transform._
import at.doml.thesis.util.collection.sized.Vec
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
    val binarized = BinarizationTransform(grayscale, canvasName)
    val groups: Option[Vec[Canvas, n.type]] =
      if (n <= 1) {
        Some(Vec.unsafeWrap(ArraySeq(binarized)))
      } else {
        SegmentationTransform(binarized, canvasName, n)
      }

    groups.map { groups =>
      val features = groups
        .mapWithIndex((c, i) => ResizeTransform(c, s"${canvasName}_${i.v}.png"))
        .mapWithIndex((c, i) => FeaturesTransform(c, s"${canvasName}_${i.v}.png"))

      labels match {
        case None     => features.mapWithIndex((fs, index) => Data.Raw(fs, canvasName, index.v))
        case Some(ls) => features.mapWithIndex((fs, index) => Data.Labeled(fs, ls(index), canvasName, index.v))
      }
    }
  }
}
