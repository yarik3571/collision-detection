package pliusnin.drawing

import pliusnin.broadphase.AABBTree
import pliusnin.collision.CollisionDetector
import pliusnin.geometry.{BoundingBox, Point}
import pliusnin.geometry.shape.Shape
import pliusnin.geometry.vector.GVector
import GVector.VectorOps



class Scene[A <: Shape[B], B <: Point[B], C <: GVector[B]] private(val shapes: Seq[A])(implicit ops: VectorOps[B, C]) {
  val boxes = shapes.head.box +: shapes.tail.map(_.box)
  val boxToShape: Map[BoundingBox[B], A] = boxes.zip(shapes).toMap
  val aabbTree = AABBTree.build(boxes.head, boxes.tail: _*)


  def add(shape: A): Scene[A, B, C] = Scene[A, B, C](shape +: shapes)

  def update(old: A, shape: A): Scene[A, B, C] =
    delete(old).add(shape)


  def delete(shape: A): Scene[A, B, C] = {
    Scene(shapes.filterNot(_ == shape))
  }

  def collisions: Map[A, C] = {
    val posCols = for {
      b <- boxes
    } yield aabbTree.findCollisions(b)
    val cols = posCols
      .flatten
      .flatMap { case (box, found) =>
        exactCollision(box, found).map(boxToShape.apply(box) -> _)
      }
      .groupBy { case (shape, _) => shape }
      .mapValues { shapesWithCollisions => shapesWithCollisions.flatMap(_._2) }
      .filter(_._2.nonEmpty)
      .mapValues(dirs => dirs.reduce { GVector.plus[B, C] })
    cols
  }

  private def exactCollision(box: BoundingBox[B], found: Seq[BoundingBox[B]]): Seq[Option[C]] = {
    val shape = boxToShape(box)
    val shapes = found
      .filterNot(_ == box)
      .map(boxToShape.apply)
    shapes.map(CollisionDetector.collision(shape, _)).map(_.asInstanceOf[Option[C]])
  }

}

object Scene {
  private def apply[A <: Shape[B], B <: Point[B], C <: GVector[B]](shapes: Seq[A])(implicit ops: VectorOps[B, C]) : Scene[A, B, C] =
    new Scene[A, B, C](shapes)

  def apply[A <: Shape[B], B <: Point[B], C <: GVector[B]](firstShape: A, shapes: Seq[A])(implicit ops: VectorOps[B, C]) : Scene[A, B, C] =
    new Scene[A, B, C](firstShape +: shapes)
}