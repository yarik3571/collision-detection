package pliusnin.geometry.vector

import pliusnin.geometry.vector.GVector.VectorOps
import pliusnin.geometry.{Point, Point2d, Point3d}

import scala.math._


sealed trait GVector[A <: Point[A]] {
  def toPoint: A
}

object GVector {

  case class Vector2d(x: Double, y: Double) extends GVector[Point2d] {
    override def toPoint: Point2d = this
  }

  case class Vector3d(x: Double, y: Double, z: Double) extends GVector[Point3d] {
    override def toPoint: Point3d = this
  }

  def negative[A <: Point[A], B <: GVector[A]](a: B)(implicit ops: VectorOps[A, B]): B = {
    ops.negative(a)
  }

  def multiply[A <: Point[A], B <: GVector[A]](a: B, d: Double)(implicit ops: VectorOps[A, B]): B = {
    ops.multiply(a, d)
  }

  def projection[A <: Point[A], B <: GVector[A]](a: B, b: B)(implicit ops: VectorOps[A, B]): B =
    ops.projection(a, b)

  def dot[A <: Point[A], B <: GVector[A]](a: B, b: B)(implicit ops: VectorOps[A, B]): Double =
    ops.dot(a, b)

  def lengthSqr[A <: Point[A], B <: GVector[A]](a: B)(implicit ops: VectorOps[A, B]): Double =
    ops.lengthSqr(a)

  def length[A <: Point[A], B <: GVector[A]](a: B)(implicit ops: VectorOps[A, B]): Double =
    sqrt(lengthSqr[A, B](a))

  def normal(vector2d: Vector2d) = Vector2d(-vector2d.y, vector2d.x)

  def normal(a: Vector3d, b: Vector3d) = Vector3d(
    a.y * b.z - a.z * b.y,
    a.z * b.x - a.x * b.z,
    a.x * b.y - a.y * b.x
  )

  def normalize[A <: Point[A], B <: GVector[A]](a: B)(implicit ops: VectorOps[A, B]): B =
    ops.normalize(a)

  def fromPoints[A <: Point[A], B <: GVector[A]](start: A, end: A)(implicit ops: VectorOps[A, B]): B =
    ops.fromPoints(start, end)

  def plus[A <: Point[A], B <: GVector[A]](a: B, b: B)(implicit ops: VectorOps[A, B]): B =
    ops.plus(a, b)

  trait VectorOps[A <: Point[A], B <: GVector[A]] {
    def negative(a: B) : B

    def multiply(a: B, d: Double) : B

    def fromPoints(start: A, end: A): B

    def dot(a: B, b: B): Double

    def lengthSqr(a: B): Double

    def length(a: B): Double

    def projection(a: B, b: B): B

    def normalize(a: B): B

    def plus(a: B, b: B) : B
  }

  implicit val vectorOpts2d: VectorOps[Point2d, Vector2d] = new VectorOps[Point2d, Vector2d] {
    override def fromPoints(start: Point2d, end: Point2d): Vector2d = Vector2d(
      end.x - start.x,
      end.y - start.y
    )

    def dot(a: Vector2d, b: Vector2d): Double =
      a.x * b.x + a.y * b.y

    override def lengthSqr(a: Vector2d): Double =
      a.x * a.x + a.y * a.y

    override def projection(a: Vector2d, b: Vector2d): Vector2d = {
      val mult = dot(a, b) / lengthSqr(b)
      Vector2d(b.x * mult, b.y * mult)
    }

    override def length(a: Vector2d): Double = sqrt(lengthSqr(a))

    override def normalize(a: Vector2d): Vector2d = {
      val len = length(a)
      Vector2d(a.x / len, a.y / len)
    }

    override def multiply(a: Vector2d, d: Double): Vector2d = Vector2d(
      a.x * d,
      a.y * d
    )

    override def plus(a: Vector2d, b: Vector2d): Vector2d = Vector2d(
      a.x + b.x,
      a.y + b.y
    )

    override def negative(a: Vector2d): Vector2d = Vector2d(
      -a.x,
      -a.y
    )
  }

  implicit val vectorOpts3d: VectorOps[Point3d, Vector3d] = new VectorOps[Point3d, Vector3d] {
    override def fromPoints(start: Point3d, end: Point3d): Vector3d = Vector3d(
      end.x - start.x,
      end.y - start.y,
      end.z - start.z
    )

    def dot(a: Vector3d, b: Vector3d): Double =
      a.x * b.x + a.y * b.y + a.z * b.z

    override def lengthSqr(a: Vector3d): Double =
      a.x * a.x + a.y * a.y + a.z * a.z

    override def projection(a: Vector3d, b: Vector3d): Vector3d = {
      val mult = dot(a, b) / lengthSqr(b)
      Vector3d(b.x * mult, b.y * mult, b.z * mult)
    }

    override def length(a: Vector3d): Double = sqrt(lengthSqr(a))

    override def normalize(a: Vector3d): Vector3d = {
      val len = length(a)
      Vector3d(a.x / len, a.y / len, a.z / len)
    }

    override def multiply(a: Vector3d, d: Double): Vector3d = Vector3d(
      a.x * d,
      a.y * d,
      a.z * d
    )

    override def plus(a: Vector3d, b: Vector3d): Vector3d = Vector3d(
      a.x + b.x,
      a.y + b.y,
      a.z + b.z
    )

    override def negative(a: Vector3d): Vector3d = Vector3d(
      -a.x,
      -a.y,
      -a.z
    )
  }


}
