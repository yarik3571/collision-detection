package pliusnin.geometry.shape

import pliusnin.geometry.shape.plane.{Circle, Polygon}
import pliusnin.geometry.shape.space.{FaceShape, Sphere}
import pliusnin.geometry.vector.GVector
import pliusnin.geometry.vector.GVector._
import pliusnin.geometry.{Point2d, Point3d}

object Projection {

  def project[A <: Shape[_], B <: GVector[_]](shape: A)(axis: B)(implicit p: Projector[A, B]): B = {
    p.project(shape)(axis)
  }

  trait Projector[A <: Shape[_], B <: GVector[_]] {
    def project(shape: A)(axis: B): B
  }

  implicit val polygonProjector = new Projector[Polygon, Vector2d] {
    override def project(shape: Polygon)(axis: Vector2d): Vector2d = {
      val verticies = shape.verticies
      val projections = verticies
        .map(v => GVector.projection[Point2d, Vector2d](v, axis))
        .map(_.toPoint)
      val projected = projections
        .combinations(2)
        .map { case Seq(f, s) => GVector.fromPoints(f, s) }
        .maxBy(GVector.lengthSqr[Point2d, Vector2d])
      projected
    }
  }

  implicit val circleProjector = new Projector[Circle, Vector2d] {
    override def project(shape: Circle)(axis: Vector2d): Vector2d = {
      val d = shape.radius * 2
      val normAxis = GVector.normalize[Point2d, Vector2d](axis)
      Vector2d(
        normAxis.x * d,
        normAxis.y * d
      )
    }
  }

  implicit val faceShapeProjector = new Projector[FaceShape, Vector3d] {
    override def project(shape: FaceShape)(axis: Vector3d): Vector3d = {
      val verticies = shape.verticies
      val projections = verticies
        .map(v => GVector.projection[Point3d, Vector3d](v, axis))
        .map(_.toPoint)
      val projected = projections
        .combinations(2)
        .map { case Seq(f, s) => GVector.fromPoints(f, s) }
        .maxBy(GVector.lengthSqr[Point3d, Vector3d])
      projected
    }
  }

  implicit val sphereProjector = new Projector[Sphere, Vector3d] {
    override def project(shape: Sphere)(axis: Vector3d): Vector3d = {
      val d = shape.radius * 2
      val normAxis = GVector.normalize[Point3d, Vector3d](axis)
      Vector3d(
        normAxis.x * d,
        normAxis.y * d,
        normAxis.z * d
      )
    }
  }

}