package pliusnin.drawing


import pliusnin.geometry.{Point, Point2d}
import pliusnin.geometry.shape.{Shape, Shape2d}
import pliusnin.geometry.shape.plane.{Circle, Polygon}
import pliusnin.geometry.vector.GVector
import pliusnin.geometry.vector.GVector.Vector2d

import scala.util.Random
import scalafx.geometry.Point2D
import scalafx.scene.paint.Color
import scalafx.scene.shape
import scalafx.scene.shape.{Ellipse, Line, Rectangle}


trait Draw2d {

  import Draw2d._

  val color = Colors(Random.nextInt(Colors.size))

  val colisionLength = 50

  def shape: Shape2d

  def scene: SceneWrapper[Shape2d, Point2d, Vector2d]


  val collisionBox = new Rectangle {
    stroke = Color.web("RED", 0.5)
    opacity = 0.1
  }

  def drawBox() = {
    val box = shape.box
    collisionBox.x = box.min.x
    collisionBox.y = box.min.y
    collisionBox.width = box.max.x - box.min.x
    collisionBox.height = box.max.y - box.min.y
  }


}

object Draw2d {
  val Colors = Seq(
    "red",
    "green",
    "blue",
    "grey"
  )
}

class PolygonDraw(var shape: Polygon,
                  val scene: SceneWrapper[Shape2d, Point2d, Vector2d])
  extends ShapeDrawInteractor with Draw2d {

  def verticies = shape.verticies

  def sideVectors = shape.sideVectors

  val sideLines = shape.sides.map { case (s, e) =>
    val line = new Line {
      stroke = Color.web("BLUE", 0.5)
      strokeWidth = 3
    }
    line.startX = s.x
    line.startY = s.y
    line.endX = e.x
    line.endY = e.y
    line
  }


  val lines = collisionBox +: sideLines

  /** Update the shape using current `start` and `end` points. */
  override def update() {
    val first = verticies.head
    val updatedStart = movePoint(first)
    val (_, upd) = sideVectors.zip(sideLines).foldLeft((updatedStart, Seq(updatedStart))) {
      case ((prev, upd), (vec, oldLine)) =>
        val x = prev.x + vec.x
        val y = prev.y + vec.y
        oldLine.startX = prev.x
        oldLine.startY = prev.y
        oldLine.endX = x
        oldLine.endY = y
        val p = Point2d(x, y)
        (p, p +: upd)
    }

    val updatedPol = Polygon(upd.head, upd(1), upd.tail.tail: _*)
    scene.update(shape, updatedPol)
    shape = updatedPol
    drawBox()
  }


  def movePoint(startP: Point2d): Point2d = {
    val deltaX = end.x - startP.x
    val deltaY = end.y - startP.y
    Point2d(
      startP.x + deltaX,
      startP.y + deltaY
    )
  }


}

class CircleDraw(var shape: Circle,
                 val scene: SceneWrapper[Shape2d, Point2d, Vector2d])
  extends ShapeDrawInteractor with Draw2d {
  val c = new scalafx.scene.shape.Circle {
    fill = Color.web("RED", 0.5)
  }

  val elems = Seq(c, collisionBox)

  /** Update the shape using current `start` and `end` points. */
  override def update() {
    c.centerX = end.x
    c.centerY = end.y
    c.radius = shape.radius
    val updated = Circle(Point2d(c.centerX.value, c.centerY.value), shape.radius)
    scene.update(shape, updated)
    shape = updated
    drawBox()
  }


}