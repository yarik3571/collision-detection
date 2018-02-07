package pliusnin

import javafx.collections.ObservableList

import pliusnin.Conf.firstTriangle
import pliusnin.CollisionDetectionMain.customScene
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



object CollisionDetectionMain extends JFXApp {

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
