package pliusnin.geometry.vector

import pliusnin.geometry.vector.GVector.{Vector2d, Vector3d}
import pliusnin.geometry.{Point2d, Point3d}
import org.scalatest.{FlatSpec, Matchers}

class VectorSpec extends FlatSpec with Matchers {

  "fromPoints" should "create vector from bounds correctly in 2d" in {
    val start = Point2d(1, 2)
    val end = Point2d(3, 4)
    val expected = Vector2d(2, 2)
    val res = GVector.fromPoints(start, end)
    res shouldBe expected
  }

  it should "create vector from bounds correctly in 3d" in {
    val start = Point3d(1, 2, 3)
    val end = Point3d(3, 4, 5)
    val expected = Vector3d(2, 2, 2)
    val res = GVector.fromPoints(start, end)
    res shouldBe expected
  }

  "normal" should "work for 2d" in {
    val vector = Vector2d(3, -1)
    val res = GVector.normal(vector)
    val expected = Vector2d(1, 3)
    res shouldBe expected
  }

  it should "work for 3d" in {
    val a = Vector3d(1, 2, 3)
    val b = Vector3d(4, 5, 6)
    val res = GVector.normal(a, b)
    val expected = Vector3d(-3, 6, -3)
    res shouldBe expected
  }

  "dot" should "calculate product correctly for 2d" in {
    val a = Vector2d(3, 4)
    val b = Vector2d(5, 6)
    val res = GVector.dot[Point2d, Vector2d](a, b)
    val expected = 39
    res shouldBe expected
  }

  it should "calculate product correctly for 3d" in {
    val a = Vector3d(3, 4, 1)
    val b = Vector3d(5, 6, 2)
    val res = GVector.dot[Point3d, Vector3d](a, b)
    val expected = 41
    res shouldBe expected
  }

  "lengthSqr" should "calculate sqr length for 2d" in {
    val a = Vector2d(3, 4)
    val res = GVector.lengthSqr[Point2d, Vector2d](a)
    val expected = 25
    res shouldBe expected
  }

  it should "calculate sqr length for 3d" in {
    val a = Vector3d(3, 4, 5)
    val res = GVector.lengthSqr[Point3d, Vector3d](a)
    val expected = 50
    res shouldBe expected
  }

  "projection" should "find projection correctly for 2d" in {
    val a = Vector2d(1, 2)
    val b = Vector2d(3, 4)
    // dot = 11, b.lengthSqr = 25
    // dot / b.length = 0.44
    //
    val res = GVector.projection[Point2d, Vector2d](a, b)
    val expected = Vector2d(1.32, 1.76)
    res shouldBe expected
  }

  it should "find projection correctly for 3d" in {
    val a = Vector3d(1, 2, 3)
    val b = Vector3d(3, 4, 5)
    // dot = 26, b.length = 50
    // dot / b.length = 0.52
    //
    val res = GVector.projection[Point3d, Vector3d](a, b)
    val expected = Vector3d(1.56, 2.08, 2.6)
    res shouldBe expected
  }

  it should "find projection of point correctly for 2d" in {
    val a = Point2d(1, 2)
    val b = Vector2d(3, 4)
    // dot = 11, b.lengthSqr = 25
    // dot / b.length = 0.44
    //
    val res: Point2d = GVector.projection[Point2d, Vector2d](a, b)
    val expected = Point2d(1.32, 1.76)
    res shouldBe expected
  }

  it should "find projection of point correctly for 3d" in {
    val a = Point3d(1, 2, 3)
    val b = Vector3d(3, 4, 5)
    // dot = 26, b.length = 50
    // dot / b.length = 0.52
    //
    val res: Point3d = GVector.projection[Point3d, Vector3d](a, b)
    val expected: Point3d = Point3d(1.56, 2.08, 2.6)
    res shouldBe expected
  }

}
