package pliusnin.geometry.shape.plane

import pliusnin.geometry.{BoundingBox, Point2d}
import org.scalatest.{FlatSpec, Matchers}

class CircleSpec extends FlatSpec with Matchers {

  "box" should "be calculated correctly" in {
    val centerX = 10
    val centerY = 10
    val radius = 3
    val circle = Circle(Point2d(centerX, centerY), radius)
    val res = circle.box
    val expected = BoundingBox(
      Point2d(centerX - radius, centerY - radius),
      Point2d(centerX + radius, centerY + radius)
    )
    res shouldBe expected
  }

}
