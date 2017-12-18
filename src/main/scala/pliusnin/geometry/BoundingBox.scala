package pliusnin.geometry


case class BoundingBox[A <: Point[A]](min: A, max: A) {
  def isIntersect(other: BoundingBox[A]): Boolean =
    BoundingBox.isIntersect(this, other)

}

object BoundingBox {

  def fromPair[A <: Point[A]](pair: (A, A)): BoundingBox[A] = pair match {
    case (min, max) => BoundingBox(min, max)
  }

  def union[A <: Point[A]](aabb: BoundingBox[A], args: BoundingBox[A]*): BoundingBox[A] = {
    val (bl, tr) = args.foldLeft((aabb.min, aabb.max)) {
      case ((min, max), elem) =>
        val updMin = min.minimizeCoordinates(elem.min)
        val updMax = max.maximizeCoordinates(elem.max)
        (updMin, updMax)
    }
    BoundingBox(bl, tr)
  }

  def isIntersect[A <: Point[A]](a: BoundingBox[A],
                                 b: BoundingBox[A]): Boolean =
    (a.max > b.min) && (b.max > a.min)


}
