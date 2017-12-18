package pliusnin.geometry

import org.scalatest.{FlatSpec, Matchers}

class BoundingBoxSpec extends FlatSpec
  with Matchers {

  import BoundingBox._

  "BoundingBox" should "find intersection if it exists in 2d" in {
    val rect1 = BoundingBox(Point2d(1, 2), Point2d(9, 10))
    val rect2 = BoundingBox(Point2d(3, 4), Point2d(11, 12))

    rect1.isIntersect(rect2) shouldBe true
    rect2.isIntersect(rect1) shouldBe true


    val rect3 = BoundingBox(Point2d(1, 1), Point2d(3, 3))
    val rect4 = BoundingBox(Point2d(2, 2), Point2d(4, 3))

    rect3.isIntersect(rect4) shouldBe true
    rect4.isIntersect(rect3) shouldBe true

  }

  it should "find intersection if it exists in 3d" in {

    val rect3d1 = BoundingBox(Point3d(1, 2, 3), Point3d(9, 10, 6))
    val rect3d2 = BoundingBox(Point3d(3, 4, 5), Point3d(5, 5, 6))

    rect3d1.isIntersect(rect3d2) shouldBe true
    rect3d2.isIntersect(rect3d1) shouldBe true
  }

  it should "not find intersection if it not exists in 2d" in {
    val rect1 = BoundingBox(Point2d(1, 2), Point2d(3, 4))
    val rect2 = BoundingBox(Point2d(5, 6), Point2d(7, 8))
    rect1.isIntersect(rect2) shouldBe false
  }

  it should "not find intersection if it not exists in 3d" in {
    val rect3d1 = BoundingBox(Point3d(1, 2, 3), Point3d(9, 10, 6))
    val rect3d2 = BoundingBox(Point3d(-1, -1, -1), Point3d(1, 2, 3))
    rect3d1.isIntersect(rect3d2) shouldBe false
  }

  it should "find intersection with point" in {
    val rect3d1 = BoundingBox(Point3d(1, 2, 3), Point3d(9, 10, 6))
    val rect3d2 = BoundingBox(Point3d(4, 4, 4), Point3d(4, 4, 4))
    rect3d1.isIntersect(rect3d2) shouldBe true
    rect3d2.isIntersect(rect3d1) shouldBe true
  }


  "union" should "union boxes correctly" in {
    val box1 = BoundingBox(Point2d(1, 2), Point2d(3, 4))
    val box2 = BoundingBox(Point2d(0, 1), Point2d(2, 3))
    val res = union(box1, box2)
    val expected = BoundingBox(Point2d(0, 1), Point2d(3, 4))
    res shouldBe expected
  }

}
