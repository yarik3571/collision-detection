package pliusnin.collision

import pliusnin.geometry.shape.Projection._
import pliusnin.geometry.shape._
import pliusnin.geometry.shape.plane.{Circle, Polygon}
import pliusnin.geometry.vector.GVector
import pliusnin.geometry.vector.GVector._
import pliusnin.geometry.{Point, Point2d, Point3d}

import scala.collection.SeqView
import GVector.{vectorOpts2d, vectorOpts3d}
import pliusnin.geometry.shape.space.{FaceShape, Sphere}


object CollisionDetector {


  def collision[A <: Point[A]](first: Shape[A], second: Shape[A]): Option[GVector[_]] = (first, second) match {
    case (a: Polygon, b: Polygon) => polPolDetector.collision(a, b)
    case (a: Polygon, b: Circle) => polCircleDetector.collision(a, b)
    case (a: Circle, b: Circle) => circleCircleDetector.collision(a, b)
    case (a: Circle, b: Polygon) => collision(b, a).map(v => GVector.negative[Point2d, Vector2d](v.asInstanceOf[Vector2d]))
    case (a: FaceShape, b: FaceShape) => fsFsDetector.collision(a, b)
    case (a: FaceShape, b: Sphere) => fsSphereDetector.collision(a, b)
    case (a: Sphere, b: Sphere) => sphereSphereDetector.collision(a, b)
    case (a: Sphere, b: FaceShape) => collision(b, a).map(v => GVector.negative[Point3d, Vector3d](v.asInstanceOf[Vector3d]))
  }

  sealed trait Detector {
    type D <: Point[D]
    type C <: GVector[D]
    type A <: Shape[D]
    type B <: Shape[D]

    implicit val aProj: Projector[A, C]
    implicit val bProj: Projector[B, C]
    implicit val vo: VectorOps[D, C]

    def collision(a: A, b: B): Option[C]

    protected[collision] def detectWithOverlays(axises: Seq[C], a: A, b: B): Option[C] = {
      val overlays = findOverlays(axises, a, b)
      if (overlays.forall(_ > 0)) {
        val direction = calcDirection(axises.zip(overlays))
        Some(direction)
      } else None
    }

    private[collision] def findProjections[E <: Shape[_]](axises: Seq[C])(p: E)(implicit projector: Projector[E, C]) = {
      axises
        .map(Projection.project[E, C](p))
    }


    private[collision] def findOverlays(axises: Seq[C], a: A, b: B): SeqView[Double, Seq[_]] = {
      val firstProjections = findProjectionLengths(axises)(a)
      val secondProjections = findProjectionLengths(axises)(b)
      val distanceVector = GVector.fromPoints[D, C](a.center, b.center)

      val doubledCenterDistances = findDoubledCenterDistances(axises)(distanceVector)
      val overlays = firstProjections
        .zip(secondProjections)
        .zip(doubledCenterDistances)
        .view
        .map { case ((fProj, sProj), distance) =>
          fProj  + sProj  - distance
        }
      overlays
    }

    private[collision] def calcDirection(axisOverlays: Seq[(C, Double)]): C = {
      val (axis, distance) = axisOverlays
        .filter { case (_, d) => d > 0 }
        .minBy { case (_, d) => d }
      val normAxis = GVector.normalize[D, C](axis)
      val directionVector = GVector.multiply[D, C](normAxis, distance)
      directionVector
    }

    private[collision] def findDoubledCenterDistances(axises: Seq[C])(distanceVector: C) =
      axises
        .map(GVector.projection[D, C](distanceVector, _))
        .map { proj =>
          2 * GVector.length[D, C](proj)
        }

    private[collision] def findProjectionLengths[E <: Shape[_]](axises: Seq[C])(p: E)(implicit projector: Projector[E, C]) = {
      axises
        .map(Projection.project[E, C](p))
        .map(a => GVector.length[D, C](a))
    }

  }

  sealed trait Detector2d extends Detector {
    type A <: Shape2d
    type B <: Shape2d
    type C = Vector2d
    type D = Point2d
    override implicit val vo: VectorOps[Point2d, Vector2d] = vectorOpts2d
  }

  sealed trait Detector3d extends Detector {
    type A <: Shape3d
    type B <: Shape3d
    type C = Vector3d
    type D = Point3d
    override implicit val vo: VectorOps[Point3d, Vector3d] = vectorOpts3d
  }

  val polPolDetector = new Detector2d {
    type A = Polygon
    type B = Polygon

    override def collision(a: Polygon, b: Polygon): Option[Vector2d] = {
      val vertAxises = for {
        v1 <- a.verticies
        v2 <- b.verticies
      } yield GVector.fromPoints[Point2d, Vector2d](v2, v1)(vo)

      val axises = a.sepAxises ++ b.sepAxises //++ vertAxises

      detectWithOverlays(axises, a, b)
    }

    override implicit val aProj: Projector[Polygon, Vector2d] = polygonProjector
    override implicit val bProj: Projector[Polygon, Vector2d] = polygonProjector
  }

  val polCircleDetector = new Detector2d {
    override type A = Polygon
    override type B = Circle

    override def collision(a: Polygon, b: Circle): Option[Vector2d] = {
      val vertVectors = a
        .verticies
          .map(v => GVector.fromPoints[D, C](v, b.center)(vo))
      val axises = vertVectors ++ a.sepAxises
      detectWithOverlays(axises, a, b)
    }

    override implicit val aProj: Projector[Polygon, Vector2d] = polygonProjector
    override implicit val bProj: Projector[Circle, Vector2d] = circleProjector
  }

  val circleCircleDetector = new Detector2d {
    override type A = Circle
    override type B = Circle
    override implicit val aProj: Projector[A, Vector2d] = circleProjector
    override implicit val bProj: Projector[A, Vector2d] = circleProjector

    override def collision(a: Circle, b: Circle): Option[Vector2d] = {
      val distanceVector = GVector.fromPoints(b.center, a.center)(vo)
      val distance = GVector.length[Point2d, Vector2d](distanceVector)(vo)
      val rSum = a.radius + b.radius
      if (distance < rSum) {
        Some(distanceVector)
      } else None
    }
  }

  val fsFsDetector = new Detector3d {
    override type A = FaceShape
    override type B = FaceShape
    override implicit val aProj: Projector[FaceShape, Vector3d] = faceShapeProjector
    override implicit val bProj: Projector[FaceShape, Vector3d] = faceShapeProjector

    override def collision(a: FaceShape, b: FaceShape): Option[Vector3d] = {
      val vertAxises = for {
        v1 <- a.verticies
        v2 <- b.verticies
      } yield GVector.fromPoints[Point3d, Vector3d](v2, v1)(vo)
      val sepAxises = a.sepAxises ++ b.sepAxises ++ vertAxises
      detectWithOverlays(sepAxises, a, b)
    }

  }

  val fsSphereDetector = new Detector3d {
    override type A = FaceShape
    override type B = Sphere

    override def collision(a: FaceShape, b: Sphere): Option[Vector3d] = {
      val closestVert = a
        .verticies
        .minBy(Point3d.distance(b.center, _))
      val circleAxis = GVector.fromPoints[D, C](b.center, closestVert)(vo)
      val axises = circleAxis +: a.sepAxises
      detectWithOverlays(axises, a, b)
    }

    override implicit val aProj: Projector[A, Vector3d] = faceShapeProjector
    override implicit val bProj: Projector[B, Vector3d] = sphereProjector
  }

  val sphereSphereDetector = new Detector3d {
    override type A = Sphere
    override type B = Sphere

    override def collision(a: A, b: B): Option[Vector3d] = {
      val distanceVector = GVector.fromPoints(b.center, a.center)(vo)
      val distance = GVector.length[Point3d, Vector3d](distanceVector)(vo)
      val rSum = a.radius + b.radius
      if (distance < rSum) {
        Some(distanceVector)
      } else None
    }

    override implicit val aProj: Projector[A, Vector3d] = sphereProjector
    override implicit val bProj: Projector[B, Vector3d] = sphereProjector
  }

}
