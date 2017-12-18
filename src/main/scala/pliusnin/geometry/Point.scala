package pliusnin.geometry

import pliusnin.geometry.vector.GVector

import scala.math._

sealed trait Point[A] {
  self: A =>

  def >(other: A): Boolean

  def <(other: A): Boolean

  def +(other: A) : A

  def distanceTo(other: A) : Double

  def minimizeCoordinates(other: A) : A

  def maximizeCoordinates(other: A) : A

}

case class Point2d(x: Double, y: Double) extends Point[Point2d] {

  import Point2d._

  override def >(other: Point2d): Boolean = bigger(this, other)

  override def <(other: Point2d): Boolean = less(this, other)

  override def +(other: Point2d): Point2d = plus(this, other)

  override def minimizeCoordinates(other: Point2d) : Point2d =
    Point2d.minimizeCoordinates(this, other)

  override def maximizeCoordinates(other: Point2d) : Point2d =
    Point2d.maximizeCoordinates(this, other)

  override def distanceTo(other: Point2d): Double = distance(this, other)


}

object Point2d {

  def distance(first: Point2d, second: Point2d): Double = {
    sqrt(pow2(first.x - second.x) + pow2(first.y - second.y))
  }

  def plus(first: Point2d, second: Point2d) = Point2d(
    first.x + second.x,
    first.y + second.y
  )

  def bigger(first: Point2d, second: Point2d): Boolean =
    first.x > second.x && first.y > second.y

  def less(first: Point2d, second: Point2d): Boolean =
    first.x < first.x && first.y < first.y

  def minimizeCoordinates(first: Point2d, second: Point2d) =
    Point2d(
      min(first.x, second.x),
      min(first.y, second.y)
    )

  def maximizeCoordinates(first: Point2d, second: Point2d) =
    Point2d(
      max(first.x, second.x),
      max(first.y, second.y)
    )
}


case class Point3d(x: Double, y: Double, z: Double) extends Point[Point3d] {
  import Point3d._

  override def >(other: Point3d): Boolean = bigger(this, other)

  override def <(other: Point3d): Boolean = less(this, other)

  override def +(other: Point3d): Point3d = plus(this, other)

  override def distanceTo(other: Point3d): Double = distance(this, other)

  override def minimizeCoordinates(other: Point3d) : Point3d =
    Point3d.minimizeCoordinates(this, other)

  override def maximizeCoordinates(other: Point3d) : Point3d =
    Point3d.maximizeCoordinates(this, other)
}

object Point3d {

  def distance(first: Point3d, second: Point3d): Double = {
    sqrt(pow2(first.x - second.x) + pow2(first.y - second.y) + pow2(first.z - second.z))
  }

  def plus(first: Point3d, second: Point3d): Point3d = Point3d(
    first.x + second.x,
    first.y + second.y,
    first.z + second.z
  )

  def bigger(first: Point3d, second: Point3d): Boolean =
    first.x > second.x && first.y > second.y && first.z > second.z

  def less(first: Point3d, second: Point3d): Boolean =
    first.x < second.x && first.y < second.y && first.z < second.z

  def minimizeCoordinates(first: Point3d, second: Point3d) =
    Point3d(
      min(first.x, second.x),
      min(first.y, second.y),
      min(first.z, second.z)
    )

  def maximizeCoordinates(first: Point3d, second: Point3d) =
    Point3d(
      max(first.x, second.x),
      max(first.y, second.y),
      max(first.z, second.z)
    )
}
