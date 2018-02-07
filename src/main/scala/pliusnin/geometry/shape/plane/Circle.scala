package pliusnin.geometry.shape.plane

import pliusnin.geometry.shape.Shape2d
import pliusnin.geometry.{BoundingBox, Point2d}

import scala.math._

case class Circle(center: Point2d, radius: Double) extends Shape2d {

  override def box: BoundingBox[Point2d] = {
    val lb = Point2d(
      center.x - radius,
      center.y - radius
    )
    val rt = Point2d(
      center.x + radius,
      center.y + radius
    )
    BoundingBox(lb, rt)
  }

  override def toString: String = s"Circle($center, $radius)"
}
