package pliusnin.broadphase

import pliusnin.broadphase.AABBTree._
import pliusnin.geometry.{BoundingBox, Point}

class AABBTree[A <: Point[A]] private[broadphase](val node: Node[A]) {

  import BoundingBox.union

  def leafs: Seq[BoundingBox[A]] = {
    def leafsRec(currentNode: Node[A]): Seq[BoundingBox[A]] = currentNode match {
      case Leaf(bBox) => Seq(bBox)
      case Branch(_, left, right, _) =>
        leafsRec(left) ++ leafsRec(right)
    }

    leafsRec(node)
  }

  def addAll(boxes: BoundingBox[A]*): AABBTree[A] =
    boxes
      .foldLeft(this) { case (res, b) => res.add(b) }

  def add(box: BoundingBox[A]): AABBTree[A] = {

    def addRec(currentNode: Node[A]): Node[A] = currentNode match {
      case Leaf(presBox) =>
        val unionBox = union(box, presBox)
        Branch(unionBox, Leaf(presBox), Leaf(box), 1)
      case Branch(_, left, right, height) if left.height < right.height =>
        val updatedLeft = addRec(left)
        val unionBox = union(updatedLeft.bBox, right.bBox)
        Branch(unionBox, updatedLeft, right, height + 1)
      case Branch(_, left, right, height) =>
        val updatedRight = addRec(right)
        val unionBox = union(left.bBox, updatedRight.bBox)
        Branch(unionBox, left, updatedRight, height + 1)
    }

    val root = addRec(node)
    AABBTree(root)
  }

  def delete(box: BoundingBox[A]): AABBTree[A] = {

    def deleteRec(currentNode: Node[A]): Node[A] = currentNode match {
      case Branch(_, Leaf(`box`), right, _) => right
      case Branch(_, left, Leaf(`box`), _) => left
      case Branch(bBox, left, right, height) if bBox.isIntersect(box) =>
        val leftUpdated = deleteRec(left)
        val rightUpdated = deleteRec(right)
        val unionBox = union(leftUpdated.bBox, rightUpdated.bBox)
        Branch(unionBox, leftUpdated, rightUpdated, height - 1)
      case default => default
    }

    val root = deleteRec(node)
    AABBTree(root)
  }

  def update(oldBox: BoundingBox[A], newBox: BoundingBox[A]): AABBTree[A] =
    delete(oldBox).add(newBox)

  def update(oldBox: BoundingBox[A])(f: BoundingBox[A] => BoundingBox[A]): AABBTree[A] =
    update(oldBox, f(oldBox))

  def findCollisions(boxes: BoundingBox[A]*): Seq[BoxWithCollisions[A]] = {
    def collisionRec(currentNode: Node[A])(box: BoundingBox[A]): Seq[BoundingBox[A]] = currentNode match {
      case Leaf(bBox) if bBox.isIntersect(box) || box.isIntersect(bBox) => Seq(bBox)
      case Branch(bBox, left, right, _) if bBox.isIntersect(box) =>
        collisionRec(left)(box) ++ collisionRec(right)(box)
      case _ => Seq()
    }

    boxes.map(b => b -> collisionRec(node)(b))
  }


  override def toString: String = s"AABBTree(${node.toString})"

}


object AABBTree {
  type BoxWithCollisions[A <: Point[A]] = (BoundingBox[A], Seq[BoundingBox[A]])

  def build[A <: Point[A]](box: BoundingBox[A],
                           boxes: BoundingBox[A]*): AABBTree[A] =
    AABBTree(box).addAll(boxes: _*)


  def apply[A <: Point[A]](boundingBox: BoundingBox[A]): AABBTree[A] =
    this.apply(Leaf(boundingBox))

  private def apply[A <: Point[A]](root: Node[A]): AABBTree[A] =
    new AABBTree(root)


  sealed trait Node[A <: Point[A]] {
    def bBox: BoundingBox[A]

    def height: Int
  }

  case class Branch[A <: Point[A]](bBox: BoundingBox[A],
                                   left: Node[A],
                                   right: Node[A],
                                   height: Int) extends Node[A]


  case class Leaf[A <: Point[A]](bBox: BoundingBox[A]) extends Node[A] {
    val height: Int = 0
  }

}
