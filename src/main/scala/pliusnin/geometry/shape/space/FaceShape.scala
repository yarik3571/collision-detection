package pliusnin.geometry.shape.space

import pliusnin.geometry.shape.Shape3d
import pliusnin.geometry.shape.space.FaceShape.Face
import pliusnin.geometry.vector.GVector
import pliusnin.geometry.vector.GVector.Vector3d
import pliusnin.geometry.{BoundingBox, Point3d}

class FaceShape private[shape](val faces: Seq[Face]) extends Shape3d {

  val verticies = faces
    .flatMap(_.points)
    .distinct

  lazy val sepAxises: Seq[Vector3d] = {
    faces
      .map {
        case Face(a, b, c, _) =>
          val ab = GVector.fromPoints(a, b)
          val ac = GVector.fromPoints(a, c)
          GVector.normal(ab, ac)
      }
  }



  override def box: BoundingBox[Point3d] = {
    verticies.zip(verticies.tail)
      .map(BoundingBox.fromPair)
      .foldLeft(BoundingBox(verticies.head, verticies.head)) {
        case (b, x) => BoundingBox.union(b, x)
      }
  }

  override def center: Point3d = {
    val centroidAcc = verticies.reduce(_ + _)
    val vertCount = verticies.size
    Point3d(
      centroidAcc.x / vertCount,
      centroidAcc.y / vertCount,
      centroidAcc.z / vertCount
    )
  }
}


object FaceShape {


  case class Face(first: Point3d, second: Point3d, other: Point3d*) {
    val points: Seq[Point3d] = first +: second +: other
  }

  def apply(first: Face, second: Face, third: Face, other: Face*): FaceShape =
    new FaceShape(first +: second +: third +: other)

}