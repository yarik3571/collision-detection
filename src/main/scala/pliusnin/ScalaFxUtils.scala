package pliusnin

import javafx.collections.ObservableList
import javafx.scene.Node

import pliusnin.drawing._
import pliusnin.geometry.Point2d
import pliusnin.geometry.shape.Shape2d
import pliusnin.geometry.vector.GVector
import pliusnin.geometry.vector.GVector.Vector2d

import scalafx.Includes._
import scalafx.scene.control.{ToggleButton, ToggleGroup}
import scalafx.scene.paint.Color
import scalafx.scene.shape.Circle

object ScalaFxUtils {
  def buildScene(shapes: Seq[Shape2d]): Scene[Shape2d, Point2d, Vector2d] = {
    drawing.Scene[Shape2d, Point2d, Vector2d](shapes.head, shapes.tail)
  }

  def buildDraws(shapes: Seq[Shape2d],
                 scene: drawing.Scene[Shape2d, Point2d, GVector.Vector2d]
                ): (SceneWrapper2d, Seq[ShapeDrawInteractor with Draw2d]) = {
    import pliusnin.geometry.shape.plane
    val sceneWrapper = new SceneWrapper2d(scene)
    val shapeDraws = shapes.map {
      case p: plane.Polygon => new PolygonDraw(p, sceneWrapper)
      case c: plane.Circle => new CircleDraw(c, sceneWrapper)
    }
    (sceneWrapper, shapeDraws)
  }

  def addToPane(children: ObservableList[Node])(draw: Draw2d): Unit = draw match {
    case pd: PolygonDraw => pd.lines.foreach(l => children += l)
    case cd: CircleDraw => cd.elems.foreach(e => children += e)
  }

  def circleToggleBtn(cToggleGroup: ToggleGroup, btnId: String)(c: CircleDraw): ToggleButton = {
    new ToggleButton {
      id = btnId
      graphic = new Circle {
        fill = Color.web(c.color, 0.5)
        radius = c.shape.radius * 0.5
      }
      toggleGroup = cToggleGroup
    }
  }

  def polygonToggleBtn(cToggleGroup: ToggleGroup, btnId: String)(p: PolygonDraw): ToggleButton = {
    new ToggleButton {
      id = btnId
      graphic = {
        val pol = new scalafx.scene.shape.Polygon()
        p.shape.verticies
          .flatMap(p => Seq(p.x * 0.5, p.y * 0.5))
          .foreach(pol.points += _)

        pol
      }
      toggleGroup = cToggleGroup
    }
  }

  def toggleBtn(cToggleGroup: ToggleGroup, btnId: String)(draw: Draw2d): ToggleButton = draw match {
    case p: PolygonDraw => polygonToggleBtn(cToggleGroup, btnId)(p)
    case c: CircleDraw => circleToggleBtn(cToggleGroup, btnId)(c)
  }
}

