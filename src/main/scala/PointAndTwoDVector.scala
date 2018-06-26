case class PointAndTwoDVector(p: Point, d: TwoDVector) {
  def toLineSegmentNormalForm: LineSegmentNormalForm = {
    val A = -d.y
    val B = d.x
    val C = -d.y * p.x + p.y * d.x
    // sx*y = sy *(x - xi) + yi *sx
    LineSegmentNormalForm(A, B, C)
  }
  def this(p1: Point, p2: Point) = this(p1, p1.pathToPoint(p2))
}

case class TwoDVector(x: Double, y: Double)