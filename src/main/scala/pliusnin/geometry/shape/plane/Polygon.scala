package pliusnin.geometry.shape.plane

import pliusnin.geometry.shape.Shape2d
import pliusnin.geometry.shape.plane.Polygon.Side
import pliusnin.geometry.vector.GVector.Vector2d
import pliusnin.geometry.{BoundingBox, Point2d, vector}

case class Polygon(first: Point2d, second: Point2d, points: Point2d*) extends Shape2d {

  val verticies: Seq[Point2d] = first +: second +: points

  lazy val center: Point2d = {

    val a = Seq(box.min,
      box.max,
      Point2d(box.min.x, box.max.y),
      Point2d(box.max.x, box.min.y))
    val centroidAcc = a.reduce(_ + _)
    val fCenter = Point2d(
      centroidAcc.x / a.size,
      centroidAcc.y / a.size
    )
    val centroidAcc1 = verticies.reduce(_ + _)
    val vertCount = verticies.size
    val sCenter = Point2d(
      centroidAcc1.x / vertCount,
      centroidAcc1.y / vertCount
    )
    Point2d(
      (fCenter.x + sCenter.x) / 2,
      (fCenter.y + sCenter.y) / 2
    )
  }

  lazy val sides: Seq[Side] = {
    val s = verticies.zip(verticies.tail)
    if (verticies.last == first) s
    else s :+ (verticies.last, first)
  }

  lazy val sideVectors: Seq[Vector2d] = {
    sides
      .map { case (start, end) => vector.GVector.fromPoints(start, end) }
  }

  lazy val sepAxises: Seq[Vector2d] =
    sideVectors.map(vector.GVector.normal)

  override val box: BoundingBox[Point2d] = {
    verticies.zip(verticies.tail)
      .map(BoundingBox.fromPair)
      .foldLeft(BoundingBox(first, second)) {
        case (b, x) => BoundingBox.union(b, x)
      }
  }

  override def toString: String = s"Polygon($verticies)"

}

object Polygon {
  type Side = (Point2d, Point2d)

  def apply(first: Point2d, second: Point2d, points: Point2d*): Polygon =
    new Polygon(first, second, points: _*)
}
