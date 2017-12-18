package pliusnin.geometry.shape.plane

import pliusnin.geometry.{BoundingBox, Point2d}
import org.scalatest.{FlatSpec, Matchers}

class CircleSpec extends FlatSpec with Matchers {

  "box" should "be calculated correctly" in {
    val circle = Circle(Point2d(10, 10), 3)
    val res = circle.box
    val expected = BoundingBox(
      Point2d(5.757359312880714, 5.757359312880714),
      Point2d(14.242640687119286, 14.242640687119286)
    )
    res shouldBe expected
  }

}
