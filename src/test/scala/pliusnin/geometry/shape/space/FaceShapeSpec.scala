package pliusnin.geometry.shape.space

import pliusnin.geometry.Point3d
import pliusnin.geometry.shape.space.FaceShape.Face
import pliusnin.geometry.vector.GVector.Vector3d
import org.scalatest.{FlatSpec, Matchers}

class FaceShapeSpec extends FlatSpec with Matchers {

  "sepAxises" should "be calculated correctly" in {
    // А (3; 0; 2),
    // В (2; 4; 5),
    // C (6; 5; 5),
    // D (7; 1; 2)
    // А1 (5; 3; 1),
    // B1 (4; 7; 4)
    // C1 (8; 8; 4)
    // D1 (9; 4; 1)
    val a = Point3d(3, 0, 2)
    val b = Point3d(2, 4, 5)
    val c = Point3d(6, 5, 5)
    val d = Point3d(7, 1, 2)
    val a1 = Point3d(5, 3, 1)
    val b1 = Point3d(4, 7, 4)
    val c1 = Point3d(8, 8, 4)
    val d1 = Point3d(9, 4, 1)

    // Faces:
    // ABCD, A1B1C1D1,
    // ABA1B1, CDC1D1
    // ADA1D1, BCB1C1
    val abcd = Face(a, b, c, d)
    val a1b1c1d1 = Face(a1, b1, c1, d1)
    val aba1b1 = Face(a, b, a1, b1)
    val cdc1d1 = Face(c, d, c1, d1)
    val ada1d1 = Face(a, d, a1, d1)
    val bcb1c1 = Face(b, c, b1, c1)
    val shape = FaceShape(
      abcd,
      a1b1c1d1,
      aba1b1,
      cdc1d1,
      ada1d1,
      bcb1c1
    )
    shape.sepAxises shouldBe Seq(
      Vector3d(-3.0, 12.0, -17.0),
      Vector3d(-3.0, 12.0, -17.0),
      Vector3d(-13.0, 5.0, -11.0),
      Vector3d(13.0, -5.0, 11.0),
      Vector3d(-1.0, 4.0, 10.0),
      Vector3d(-1.0, 4.0, 10.0)
    )
  }

}
