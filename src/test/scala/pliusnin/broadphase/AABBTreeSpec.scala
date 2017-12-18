package pliusnin.broadphase

import org.scalatest.{FlatSpec, Matchers}
import pliusnin.broadphase.AABBTree
import pliusnin.geometry.{BoundingBox, Point2d}

class AABBTreeSpec extends FlatSpec with Matchers {

  import AABBTree._

  "add" should "add BoundaryRectangles correctly" in {
    val firstBox = BoundingBox(Point2d(4, 5), Point2d(6, 7))
    val secondBox = BoundingBox(Point2d(3, 3), Point2d(4, 4))
    val tree = AABBTree(firstBox)
    val added1 = tree.add(secondBox)

    val expectedNode1 = Branch(
      BoundingBox(Point2d(3, 3), Point2d(6, 7)),
      Leaf(firstBox),
      Leaf(secondBox),
      1
    )
    added1.node shouldBe expectedNode1

    val thirdBox = BoundingBox(Point2d(1, 1), Point2d(2, 2))
    val added2 = added1.add(thirdBox)

    val expectedNode2 = Branch(
      BoundingBox(Point2d(1.0, 1.0), Point2d(6.0, 7.0)),
      Leaf(BoundingBox(Point2d(4.0, 5.0), Point2d(6.0, 7.0))),
      Branch(
        BoundingBox(Point2d(1.0, 1.0), Point2d(4.0, 4.0)),
        Leaf(BoundingBox(Point2d(3.0, 3.0), Point2d(4.0, 4.0))),
        Leaf(BoundingBox(Point2d(1.0, 1.0), Point2d(2.0, 2.0))),
        1),
      2)
    added2.node shouldBe expectedNode2
  }


  "findCollisions" should "find collisions for elements in tree" in {
    val firstBox = BoundingBox(Point2d(4, 5), Point2d(6, 7))
    val secondBox = BoundingBox(Point2d(3, 3), Point2d(4, 4))
    val thirdBox = BoundingBox(Point2d(1, 1), Point2d(2, 2))
    val root = build(firstBox, secondBox, thirdBox)
    root.findCollisions(firstBox) shouldBe Seq(firstBox -> Seq(firstBox))
    root.findCollisions(secondBox) shouldBe Seq(secondBox -> Seq(secondBox))
    root.findCollisions(thirdBox) shouldBe Seq(thirdBox -> Seq(thirdBox))
  }

  it should "find collisions for elements not in tree" in {
    val firstBox = BoundingBox(Point2d(4, 5), Point2d(6, 7))
    val secondBox = BoundingBox(Point2d(3, 3), Point2d(4, 4))
    val thirdBox = BoundingBox(Point2d(1, 1), Point2d(3, 3))
    val root = build(firstBox, secondBox, thirdBox)

    val testBox = BoundingBox(Point2d(2, 2), Point2d(10, 10))
    val expected = Seq(
      testBox -> Seq(firstBox, secondBox, thirdBox)
    )
    root.findCollisions(testBox) shouldBe expected
  }


  "delete" should "delete leafs correctly" in {
    val firstBox = BoundingBox(Point2d(4, 5), Point2d(6, 7))
    val secondBox = BoundingBox(Point2d(3, 3), Point2d(4, 4))
    val thirdBox = BoundingBox(Point2d(1, 1), Point2d(2, 2))
    val root = build(firstBox, secondBox, thirdBox)
    val res1 = root.delete(firstBox)
    val expected1 =
      Branch(BoundingBox(Point2d(1.0,1.0),Point2d(4.0,4.0)),Leaf(BoundingBox(Point2d(3.0,3.0),Point2d(4.0,4.0))),Leaf(BoundingBox(Point2d(1.0,1.0),Point2d(2.0,2.0))),1)
    res1.node shouldBe expected1
    res1.leafs.length shouldBe 2
    res1.leafs.forall(Seq(secondBox, thirdBox).contains) shouldBe true

    val res2 = root.delete(secondBox)
    res2.leafs.length shouldBe 2
    res2.leafs.forall(Seq(firstBox, thirdBox).contains) shouldBe true

    val res3 = root.delete(thirdBox)
    res3.leafs.length shouldBe 2
    res3.leafs.forall(Seq(firstBox, secondBox).contains) shouldBe true

    val res4 = root.delete(firstBox).delete(secondBox)
    res4.leafs.length shouldBe 1
    res4.leafs.forall(Seq(thirdBox).contains) shouldBe true

  }
}
