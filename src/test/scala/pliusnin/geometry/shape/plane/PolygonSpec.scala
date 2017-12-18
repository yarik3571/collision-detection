package pliusnin.geometry.shape.plane

import pliusnin.geometry.{BoundingBox, Point2d, vector}
import org.scalatest.{FlatSpec, Matchers}

class PolygonSpec extends FlatSpec with Matchers {
  "sides" should "be calculated correctly" in {
    val firstP = Point2d(1, 2)
    val secondP = Point2d(3, 4)
    val thirdP = Point2d(5, 6)
    val polygon = Polygon(firstP, secondP, thirdP)
    val res = polygon.sides
    res shouldBe Seq(firstP -> secondP, secondP -> thirdP, thirdP -> firstP)
  }

  "sideVectors" should "be calculated correctly" in {
    val firstP = Point2d(1, 2)
    val secondP = Point2d(3, 4)
    val thirdP = Point2d(5, 6)
    val polygon = Polygon(firstP, secondP, thirdP)
    val res = polygon.sideVectors
    res shouldBe Seq(
      vector.GVector.fromPoints(firstP, secondP),
      vector.GVector.fromPoints(secondP, thirdP),
      vector.GVector.fromPoints(thirdP, firstP)
    )
  }

  "sepAxises" should "be calculated correctly" in {
    val polygon = Polygon(Point2d(1, 2), Point2d(3, 4), Point2d(5, 6))
    val res = polygon.sepAxises
    res shouldBe polygon.sideVectors.map(vector.GVector.normal)
  }

  "box" should "be calculated correctly" in {
    val firstP = Point2d(1, 2)
    val secondP = Point2d(3, 4)
    val thirdP = Point2d(5, 6)
    val polygon = Polygon(firstP, secondP, thirdP)
    val res = polygon.box
    res shouldBe BoundingBox(firstP, thirdP)
  }




}
