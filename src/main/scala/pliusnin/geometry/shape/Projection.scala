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

  def minMaxScalar[A <: Shape[_], B <: GVector[_]](shape: A)(axis: B)(implicit p: Projector[A, B]): (Double, Double) = {
    p.minMaxScalar(shape)(axis)
  }

  trait Projector[A <: Shape[_], B <: GVector[_]] {
    def project(shape: A)(axis: B): B
    def minMaxScalar(shape: A)(axis: B): (Double, Double)
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

    override def minMaxScalar(shape: Polygon)(axis: Vector2d): (Double, Double) = {
      val vectors = shape.verticies.map(p => Vector2d(p.x, p.y))
      val firstScalar = GVector.dot[Point2d, Vector2d](vectors.head, axis)
      vectors.foldLeft((firstScalar, firstScalar)) {
        case ((min, max), v) =>
          val vScalar = GVector.dot[Point2d, Vector2d](v, axis)
          val minUpd = if (vScalar < min) vScalar else min
          val maxUpd = if (vScalar > max) vScalar else max
          (minUpd, maxUpd)
      }
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

    override def minMaxScalar(shape: Circle)(axis: Vector2d): (Double, Double) = {
      val d = shape.radius * 2
      val normAxis = GVector.normalize[Point2d, Vector2d](axis)
      val start =  Vector2d(
        normAxis.x * d,
        normAxis.y * d
      )
      val startScalar = GVector.dot[Point2d, Vector2d](start, axis)
      (startScalar, startScalar)
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

    override def minMaxScalar(shape: FaceShape)(axis: Vector3d): (Double, Double) = {
      val vectors = shape.verticies.map(p => Vector3d(p.x, p.y, p.z))
      val firstScalar = GVector.dot[Point3d, Vector3d](vectors.head, axis)
      vectors.foldLeft((firstScalar, firstScalar)) {
        case ((min, max), v) =>
          val vScalar = GVector.dot[Point3d, Vector3d](v, axis)
          val minUpd = if (vScalar < min) vScalar else min
          val maxUpd = if (vScalar > max) vScalar else max
          (minUpd, maxUpd)
      }
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

    override def minMaxScalar(shape: Sphere)(axis: Vector3d): (Double, Double) = {
      val r = shape.radius
      val normAxis = GVector.normalize[Point3d, Vector3d](axis)
      val start =  Vector3d(
        normAxis.x * r,
        normAxis.y * r,
        normAxis.z * r
      )
      val end = Vector3d(
        normAxis.x * -r,
        normAxis.y * -r,
        normAxis.z * -r
      )
      val startScalar = GVector.dot[Point3d, Vector3d](start, axis)
      val endScalar = GVector.dot[Point3d, Vector3d](end, axis)

      val minScalar = math.min(startScalar, endScalar)
      val maxScalar = math.max(startScalar, endScalar)
      (minScalar, maxScalar)
    }
  }

}
