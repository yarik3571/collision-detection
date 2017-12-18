package pliusnin.geometry.shape

import pliusnin.geometry._

trait Shape[A <: Point[A]] {
  def box: BoundingBox[A]
  def center: A
}

trait Shape2d extends Shape[Point2d] {

}

trait Shape3d extends Shape[Point3d] {

}


