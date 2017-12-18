//package pliusnin.geometry
//
//import Scene._
//
//trait Scene {
//
//  val movingObjects: MovingObjects
//  val staticObjects: StaticObjects
//
//  lazy val objects: Seq[ShapeWithCoordinates] = {
//    val moving = movingObjects
//      .map { case (shape, coords, _) => (shape, coords) }
//
//    moving ++ staticObjects
//  }
//
//  def tick() = {
//
//  }
//
//}
//
//object Scene {
//  type MovingObjects = Seq[ShapeWithCoordinatesAndSpeed]
//  type StaticObjects = Seq[ShapeWithCoordinates]
//}
//
//
//
