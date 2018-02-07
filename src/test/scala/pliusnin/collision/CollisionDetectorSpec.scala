package pliusnin.collision

import pliusnin.geometry.{Point2d, Point3d}
import pliusnin.geometry.shape.plane._
import pliusnin.geometry.shape.space.{FaceShape, Sphere}
import pliusnin.geometry.shape.space.FaceShape.Face
import pliusnin.geometry.vector.GVector.{Vector2d, Vector3d}
import org.scalatest.{FlatSpec, Matchers}

class CollisionDetectorSpec extends FlatSpec with Matchers {

  import CollisionDetector._

  "detectCollision2d" should "find collision for two rectangles" in {
    val first = Polygon(
      Point2d(1, 3),
      Point2d(1, 5),
      Point2d(3, 5),
      Point2d(3, 3)
    )
    val second = Polygon(
      Point2d(0, 1),
      Point2d(0, 4),
      Point2d(2, 4),
      Point2d(2, 1)
    )
    val res = collision(first, second)
    res.isDefined shouldBe true
  }

  it should "find collision for polygons if it exists" in {
    val first = Polygon(
      Point2d(0, 0),
      Point2d(1, 2),
      Point2d(2, 1),
      Point2d(2, 2),
      Point2d(1, 1)
    )
    val second = Polygon(
      Point2d(-1, -1),
      Point2d(1.5, 1.5),
      Point2d(30, 4),
      Point2d(-5, -6)
    )

    val res = collision(first, second)
    res.isDefined shouldBe true
  }


  it should "not find collision for two triangles if it not exists" in {
    val first = Polygon(
      Point2d(2, 2),
      Point2d(2, 3),
      Point2d(3, 3)
    )
    val second = Polygon(
      Point2d(4, 4),
      Point2d(4, 6),
      Point2d(7, 6)
    )
    val res = collision(first, second)
    res shouldBe None
    res.isDefined shouldBe false
  }

  it should "not find collision for two triangles if it not exists 2" in {
    val first = Polygon(
      Point2d(2, 2),
      Point2d(2, 3),
      Point2d(3, 3)
    )
    val second = Polygon(
      Point2d(10, 10),
      Point2d(10, 11),
      Point2d(12, 11)
    )
    val res = collision(first, second)
    res shouldBe None
    res.isDefined shouldBe false
  }

  it should "not find collision for two rectangles if it not exists" in {
    val first = Polygon(
      Point2d(1, 3),
      Point2d(1, 5),
      Point2d(3, 5),
      Point2d(3, 3)
    )
    val second = Polygon(
      Point2d(10, 10),
      Point2d(10, 11),
      Point2d(11, 11),
      Point2d(11, 10)
    )
    val res = collision(first, second)
    res.isDefined shouldBe false
  }

  it should "not find collision for two rectangles if it not exists 2" in {
    val first = Polygon(
      Point2d(1, 3),
      Point2d(1, 5),
      Point2d(3, 5),
      Point2d(3, 3)
    )
    val second = Polygon(
      Point2d(1000, 1000),
      Point2d(1000, 1100),
      Point2d(1100, 1100),
      Point2d(1100, 1000)
    )
    val res = collision(first, second)
    res.isDefined shouldBe false
  }

  it should "not find collision for two triangle if it not exists" in {
    val first = Polygon(
      Point2d(1, 3),
      Point2d(1, 5),
      Point2d(3, 5)
    )
    val second = Polygon(
      Point2d(1000, 1000),
      Point2d(1000, 1100),
      Point2d(1100, 1100)
    )
    val res = collision(first, second)
    res shouldBe None
  }

  it should "find collision for two circles if it exists" in {
    val first = Circle(Point2d(1, 1), 5)
    val second = Circle(Point2d(4, 4), 4)
    val res = collision(first, second)
    res.isDefined shouldBe true
    res shouldBe Some(Vector2d(-3, -3))
  }

  it should "not find collision for two circles if it not exists" in {
    val first = Circle(Point2d(1, 1), 1)
    val second = Circle(Point2d(4, 4), 1)
    val res = collision(first, second)
    res.isDefined shouldBe false
  }

  it should "find collision for rectangle and circle" in {
    val rect = Polygon(
      Point2d(1, 3),
      Point2d(1, 5),
      Point2d(3, 5),
      Point2d(3, 3)
    )
    val circle = Circle(Point2d(2, 2), 2)
    val res = collision(rect, circle)
    res.isDefined shouldBe true


  }

  it should "not find collision for rectangle and circle if it not exists" in {
    val rect = Polygon(
      Point2d(1, 3),
      Point2d(1, 5),
      Point2d(3, 5),
      Point2d(3, 3)
    )
    val circle = Circle(Point2d(100, 100), 2)
    val res1 = collision(rect, circle)
    val res2 = collision(circle, rect)
    res1 shouldBe res2
    res1.isDefined shouldBe false

    //    detectCollision2d(circle, rect) shouldBe false
  }

  def moveFaceShape(fs: FaceShape, dir: Vector3d): FaceShape = {
    val secondFaces = fs.faces.map { f =>
      val updP = f.points.map(p => Point3d(p.x + dir.x, p.y + dir.y, p.z + dir.z))
      Face(updP.head, updP(1), updP.drop(2): _*)
    }
    val res = FaceShape(secondFaces(0), secondFaces(1), secondFaces(2), secondFaces.drop(3): _*)

    fs.faces.size shouldBe res.faces.size
    val copiedCorrectly = fs.faces.zip(res.faces).forall {
      case (a, b) => a.points.lengthCompare(b.points.size) == 0
    }
    assert(copiedCorrectly)
    res
  }

  def moveSphere(sphere: Sphere, dir: Vector3d): Sphere = Sphere(
    Point3d(sphere.center.x + dir.x, sphere.center.y + dir.y, sphere.center.z + dir.z),
    sphere.radius
  )

  val defaultParallelipiped: FaceShape = {
    // А (3; 0; 2),
    // В (2; 4; 5),
    // C (6; 5; 5),
    // D (7; 1; 2)
    // А1 (5; 3; 1),
    // B1 (4; 7; 4)
    // C1 (8; 8; 4)
    // D1 (9; 4; 1)
    val a = Point3d(3, 0, 2)
    val b = Point3d(2, 4, 5)
    val c = Point3d(6, 5, 5)
    val d = Point3d(7, 1, 2)
    val a1 = Point3d(5, 3, 1)
    val b1 = Point3d(4, 7, 4)
    val c1 = Point3d(8, 8, 4)
    val d1 = Point3d(9, 4, 1)

    // Faces:
    // ABCD, A1B1C1D1,
    // ABA1B1, CDC1D1
    // ADA1D1, BCB1C1
    val abcd = Face(a, b, c, d)
    val a1b1c1d1 = Face(a1, b1, c1, d1)
    val aba1b1 = Face(a, b, a1, b1)
    val cdc1d1 = Face(c, d, c1, d1)
    val ada1d1 = Face(a, d, a1, d1)
    val bcb1c1 = Face(b, c, b1, c1)
    FaceShape(
      abcd,
      a1b1c1d1,
      aba1b1,
      cdc1d1,
      ada1d1,
      bcb1c1
    )
  }
  val defaultSphere = Sphere(Point3d(3, 3, 3), 1)

  val second = Polygon(
    Point2d(-1, -1),
    Point2d(1.5, 1.5),
    Point2d(30, 4),
    Point2d(-5, -6)
  )

  it should "find collsion for parallelipiped if exists" in {
    val first = defaultParallelipiped
    val second = moveFaceShape(first, Vector3d(1, 1, 1))
    val res = collision(first, second)
    res.isDefined shouldBe true
  }

  it should "not find collision for parallelipiped if it not exists" in {
    val first = defaultParallelipiped
    val second = moveFaceShape(first, Vector3d(100, 100, 100))
    val res = collision(first, second)
    res shouldBe None
  }


  it should "not find collision for parallelipiped and sphere if it not exists" in {
    val parallelipiped = defaultParallelipiped
    val sphere = moveSphere(defaultSphere, Vector3d(100, 100, 100))
    val res = collision(parallelipiped, sphere)
    res shouldBe None
  }

  it should "find collision for two spheres it exists" in {
    val first = defaultSphere
    val second = moveSphere(first, Vector3d(0.5, 0.5, 0.5))
    val res = collision(first, second)
    res.isDefined shouldBe true
  }

  it should "not find collision for two spheres it not exists" in {
    val first = defaultSphere
    val second = moveSphere(first, Vector3d(2, 2, 2))
    val res = collision(first, second)
    res shouldBe None
  }




}
