package example.scalafx

import scalafx.Includes._
import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.geometry.Point2D
import scalafx.scene.Scene
import scalafx.scene.input.MouseEvent
import scalafx.scene.paint.Color
import scalafx.stage.{WindowEvent, StageStyle}

object DraggableApp extends JFXApp {

  private var anchorPt: Point2D = null
  private var previousLocation: Point2D = null

  stage = new PrimaryStage {
    initStyle(StageStyle.TRANSPARENT)
    scene = new Scene {
      fill = Color.rgb(0, 0, 0, 1.0)
    }
  }

  // Initialize stage to be movable via mouse
  initMovablePlayer()

  /**
    * Initialize the stage to allow the mouse cursor to move the application
    * using dragging.
    */
  private def initMovablePlayer(): Unit = {
    val scene = stage.getScene

    scene.onMousePressed = (event: MouseEvent) => anchorPt = new Point2D(event.screenX, event.screenY)

    scene.onMouseDragged = (event: MouseEvent) =>
      if (anchorPt != null && previousLocation != null) {
        stage.x = previousLocation.x + event.screenX - anchorPt.x
        stage.y = previousLocation.y + event.screenY - anchorPt.y
      }

    scene.onMouseReleased = (event: MouseEvent) => previousLocation = new Point2D(stage.getX, stage.getY)

    stage.onShown = (event: WindowEvent) => previousLocation = new Point2D(stage.getX, stage.getY)
  }
}
