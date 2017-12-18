package pliusnin

import javafx.collections.ObservableList

import pliusnin.Conf.firstTriangle
import pliusnin.Main.customScene
import pliusnin.drawing._
import pliusnin.geometry.Point2d
import pliusnin.geometry.shape.Shape2d
import pliusnin.geometry.shape.plane.Polygon
import pliusnin.geometry.vector.GVector
import pliusnin.geometry.vector.GVector.Vector2d

import scala.collection.TraversableOnce
import scalafx.Includes._
import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.event.subscriptions.Subscription
import scalafx.scene.{Scene, shape}
import javafx.scene.Node
import scalafx.scene.control.{ToggleButton, ToggleGroup, ToolBar}
import scalafx.scene.input.MouseEvent
import scalafx.scene.layout.{BorderPane, Pane}
import scalafx.scene.paint.Color
import scalafx.scene.shape._

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

object ScalaFxUtils {
  def buildScene(shapes: Seq[Shape2d]) = {
    drawing.Scene[Shape2d, Point2d, Vector2d](shapes.head, shapes.tail)
  }

  def buildDraws(shapes: Seq[Shape2d],
                 scene: drawing.Scene[Shape2d, Point2d, GVector.Vector2d]) = {
    import pliusnin.geometry.shape.plane
    val sceneWrapper = new SceneWrapper2d(scene)
    val shapeDraws = shapes.map {
      case p: plane.Polygon => new PolygonDraw(p, sceneWrapper)
      case c: plane.Circle => new CircleDraw(c, sceneWrapper)
    }
    (sceneWrapper, shapeDraws)
  }

  def addToPane(children: ObservableList[Node])(draw: Draw2d) = draw match {
    case pd: PolygonDraw => pd.lines.foreach(l => children += l)
    case cd: CircleDraw => cd.elems.foreach(e => children += e)
  }

  def circleToggleBtn(cToggleGroup: ToggleGroup, btnId: String)(c: CircleDraw) = {
    new ToggleButton {
      id = btnId
      graphic = new Circle {
        fill = Color.web(c.color, 0.5)
        radius = c.shape.radius * 0.5
      }
      toggleGroup = cToggleGroup
    }
  }

  def polygonToggleBtn(cToggleGroup: ToggleGroup, btnId: String)(p: PolygonDraw) = {
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

  def toggleBtn(cToggleGroup: ToggleGroup, btnId: String)(draw: Draw2d) = draw match {
    case p: PolygonDraw => polygonToggleBtn(cToggleGroup, btnId)(p)
    case c: CircleDraw => circleToggleBtn(cToggleGroup, btnId)(c)
  }
}

object Main extends JFXApp {

  import Conf._
  import ScalaFxUtils._


  val customScene = buildScene(shapes)
  val (sceneWrapper, shapeDraws) = buildDraws(shapes, customScene)
  val drawsWithIds = (1 to shapeDraws.size).map(_.toString).zip(shapeDraws).toMap
  val drawingPane = new Pane {
    shapeDraws.foreach(addToPane(children))
    sceneWrapper.collisionLines.values.foreach(e => children += e)
  }




  stage = new PrimaryStage {
    title = "Collision detection"
    scene = new Scene(Conf.width, Conf.height) {
      root = new BorderPane {
        top = new ToolBar {
          val alignToggleGroup = new ToggleGroup()
          content = drawsWithIds.map {
            case (id, draw) => toggleBtn(alignToggleGroup, id.toString)(draw)
          }


          // Subscription to the current mouse event handler
          var mouseHandlerSubscription: Option[Subscription] = None

          // Handle pressing to toggle buttons.
          alignToggleGroup.selectedToggle.onChange {
            // Cancel current mouse event handler
            mouseHandlerSubscription.foreach(_.cancel())
            // Determine which shape is selected
            val handlerId = alignToggleGroup.selectedToggle().asInstanceOf[javafx.scene.control.ToggleButton].id()
            val selectedHandler = drawsWithIds.get(handlerId).map(_.handler)

            // Selected corresponding handler
            mouseHandlerSubscription = selectedHandler match {
              case Some(h) => Some(drawingPane.handleEvent(MouseEvent.Any)(h))
              case None => None
            }
          }

          // Select first button. We do selection here, after the handling of button selection was defined,
          // so this initial selection can be handled the same way as any other selection.
          alignToggleGroup.selectToggle(alignToggleGroup.toggles(0))
        }
        center = drawingPane
      }
    }
  }

}
