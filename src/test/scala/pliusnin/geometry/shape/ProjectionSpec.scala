package pliusnin.geometry.shape

import pliusnin.geometry.Point2d
import pliusnin.geometry.shape.plane._
import pliusnin.geometry.vector.GVector.Vector2d
import org.scalatest.{FlatSpec, Matchers}

class ProjectionSpec extends FlatSpec with Matchers {

  import Projection._

  "projectPolygon" should "project square on X correctly" in {
    val square = Polygon(Point2d(1, 1), Point2d(1, 2), Point2d(2, 2), Point2d(2, 1))
    val axis = Vector2d(1, 0)
    val res = polygonProjector.project(square)(axis)
    val expected = Vector2d(1, 0)
    res shouldBe expected
  }

  "projectPolygon" should "project square on Y correctly" in {
    val square = Polygon(Point2d(1, 1), Point2d(1, 2), Point2d(2, 2), Point2d(2, 1))
    val axis = Vector2d(0, 1)
    val res = polygonProjector.project(square)(axis)
    val expected = Vector2d(0, 1)
    res shouldBe expected
  }

  "projectPolygon" should "project square on custom axis correctly 1" in {
    val square = Polygon(Point2d(1, 1), Point2d(1, 2), Point2d(2, 2), Point2d(2, 1))
    val axis = Vector2d(1, 1)
    val res = polygonProjector.project(square)(axis)
    val expected = Vector2d(1, 1)
    res shouldBe expected
  }

  "projectPolygon" should "project triangle on custom axis correctly" in {
    val square = Polygon(Point2d(1, 1), Point2d(1, 2), Point2d(2, 1))
    val axis = Vector2d(-1, -1)
    val res = polygonProjector.project(square)(axis)
    val expected = Vector2d(0.5, 0.5)
    res shouldBe expected
  }

  "projectCircle" should "project circle correctly on X" in {
    val center = Point2d(1, 1)
    val radius = 2
    val circle = Circle(center, radius)
    val axis = Vector2d(1, 0)
    val res = circleProjector.project(circle)(axis)
    val expected = Vector2d(4, 0)
    res shouldBe expected
  }

  it should "project circle correctly on Y" in {
    val center = Point2d(1, 1)
    val radius = 2
    val circle = Circle(center, radius)
    val axis = Vector2d(0, 1)
    val res = circleProjector.project(circle)(axis)
    val expected = Vector2d(0, 4)
    res shouldBe expected
  }

  it should "project circle correctly on custom" in {
    val center = Point2d(1, 1)
    val radius = 2
    val circle = Circle(center, radius)
    val axis = Vector2d(1, 1)
    val res = circleProjector.project(circle)(axis)
    val expected = Vector2d(2.82842712474619,2.82842712474619)
    res shouldBe expected
  }



}
