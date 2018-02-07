package pliusnin

import pliusnin.geometry.Point2d
import pliusnin.geometry.shape.plane.Polygon

//noinspection ScalaStyle
object Conf {

  import pliusnin.geometry.shape.plane

  val width: Double = 1500
  val height: Double = 1000

  private val firstTriangle = Polygon(Point2d(1, 1), Point2d(100, 1), Point2d(100, 100))
  private val secondTriangle = Polygon(Point2d(100, 100), Point2d(150, 150), Point2d(200, 100))
  private val rectangle = Polygon(Point2d(1, 1), Point2d(100, 1), Point2d(100, 100), Point2d(1, 100))
  private val firstPolygon = Polygon(
    Point2d(200, 200),
    Point2d(150, 150),
    Point2d(100, 150),
    Point2d(100, 200),
    Point2d(150, 200)
  )
  private val secondPolygon = Polygon(
    Point2d(400, 400),
    Point2d(300, 300),
    Point2d(100, 300),
    Point2d(200, 400),
    Point2d(300, 500)
  )
  private val firstCircle = plane.Circle(Point2d(width / 2, height / 2), width / 50d)
  private val secondCircle = plane.Circle(Point2d(width / 2, height / 2), width / 20d)

  val shapes = Seq(
    firstTriangle,
    secondTriangle,
    rectangle,
    firstPolygon,
    secondPolygon,
    firstCircle,
    secondCircle
  )
}

