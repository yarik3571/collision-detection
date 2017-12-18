package pliusnin.geometry

import pliusnin.geometry.vector.GVector.{Vector2d, Vector3d}

package object vector {

  implicit def point2dToVector(p : Point2d): Vector2d =
    Vector2d(p.x, p.y)

  implicit def point3dToVector(p : Point3d): Vector3d =
    Vector3d(p.x, p.y, p.z)


  implicit def vector2dToPoint(vector : Vector2d): Point2d =
    Point2d(vector.x, vector.y)

  implicit def vector3dToPoint(vector : Vector3d): Point3d =
    Point3d(vector.x, vector.y, vector.z)
}
