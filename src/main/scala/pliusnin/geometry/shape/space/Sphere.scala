package pliusnin.geometry.shape.space

import pliusnin.geometry.shape.Shape3d
import pliusnin.geometry.{BoundingBox, Point3d}

import scala.math._


class Sphere(val center: Point3d, val radius: Double) extends Shape3d {

  override def box: BoundingBox[Point3d] = {
    val boxSide = radius * 2
    val distanceToVertice = sqrt(2) * boxSide / 2
    val lb = Point3d(
      center.x - distanceToVertice,
      center.y - distanceToVertice,
      center.z - distanceToVertice
    )
    val rt = Point3d(
      center.x + distanceToVertice,
      center.y + distanceToVertice,
      center.z - distanceToVertice
    )
    BoundingBox(lb, rt)
  }

}

object Sphere {
  def apply(center: Point3d, radius: Double): Sphere =
    new Sphere(center, radius)
}
