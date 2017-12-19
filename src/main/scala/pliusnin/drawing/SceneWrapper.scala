package pliusnin.drawing

import pliusnin.geometry.{Point, Point2d}
import pliusnin.geometry.shape.{Shape, Shape2d}
import pliusnin.geometry.vector.GVector
import pliusnin.geometry.vector.GVector.Vector2d

import scala.collection.mutable
import scalafx.scene.paint.Color
import scalafx.scene.shape.Line

abstract class SceneWrapper[A <: Shape[B], B <: Point[B], C <: GVector[B]](_scene: Scene[A, B, C]) {
  private var scene: Scene[A, B, C] = _scene

  val collisionLength = 100

  val collisionLines = mutable.Map[A, Line]()

  var index = 0
  val colors = Seq(
    "RED", "GREEN", "BLUE", "BLACK"
  )

  scene.shapes.foreach(add)

  def add(shape: A) = {
    collisionLines.put(shape, new Line {
      stroke = Color.web(colors(index % colors.size), 0.5)
      strokeWidth = 3
    })
    index += 1
    scene = scene.add(shape)
  }

  def update(old: A, shape: A) = synchronized {
    val a = collisionLines.remove(old)
    a.foreach(line => collisionLines.put(shape, line))

    scene = scene.delete(old).add(shape)
    updateCollisions()
  }

  def resetLine(line: Line)

  private def resetLines() =
    collisionLines.foreach { case (_, line) => resetLine(line) }

  def updateLine(shape: A, dir: C)

  def updateCollisions(): Unit = {
    println("Searching for collisions")
    resetLines()
    scene.collisions.foreach {
      case (s, dir) => updateLine(s, dir)
        println(s"Collsion found $dir for $s")
    }
  }


}

class SceneWrapper2d(_scene: Scene[Shape2d, Point2d, Vector2d]) extends SceneWrapper(_scene) {
  override def resetLine(line: Line): Unit = {
    line.startX = 1
    line.startY = 1
    line.endX = 1
    line.endY = 1
  }

  override def updateLine(shape: Shape2d, dir: Vector2d): Unit = {
    val line = collisionLines(shape)
    val normCol = GVector.normalize[Point2d, Vector2d](Vector2d(dir.x, dir.y))
    val multiplied = GVector.multiply[Point2d, Vector2d](normCol, collisionLength)
    val end = Point2d(multiplied.x + shape.center.x, multiplied.y + shape.center.y)
    line.startX = shape.center.x
    line.startY = shape.center.y
    line.endX = end.x
    line.endY = end.y
  }
}
