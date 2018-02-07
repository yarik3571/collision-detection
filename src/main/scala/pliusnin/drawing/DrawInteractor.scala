package pliusnin.drawing

import scalafx.Includes._
import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.event.subscriptions.Subscription
import scalafx.geometry.Point2D
import scalafx.scene.Scene
import scalafx.scene.control.{ToggleButton, ToggleGroup, ToolBar}
import scalafx.scene.input.MouseEvent
import scalafx.scene.layout.{BorderPane, Pane}
import scalafx.scene.paint.Color
import scalafx.scene.shape.{Circle, Ellipse, Line, Rectangle}


trait MouseHandler {
  /** Return event handling method */
  def handler: MouseEvent => Unit
}

/** Encapsulates common behaviour of interaction when drawing a shape based on two points. */
trait ShapeDrawInteractor extends MouseHandler {
  private var _start = new Point2D(0, 0)
  private var _end = new Point2D(0, 0)

  def start: Point2D = _start

  def start_=(p: Point2D): Unit = {
    _start = p
    _end = p
    update()
  }

  def end: Point2D = _end

  def end_=(p: Point2D) {
    _end = p
    update()
  }

  /** Update the shape using current `start` and `end` points. */
  def update()

  override def handler: MouseEvent => Unit = {
    me: MouseEvent => {
      me.eventType match {
        case MouseEvent.MousePressed => start = new Point2D(me.x, me.y)
        case MouseEvent.MouseDragged => end = new Point2D(me.x, me.y)
        case _ => {}
      }
    }
  }
}

