object LineSegmentNormalForm {
  def lineIntersection(l1: LineSegmentNormalForm, l2: LineSegmentNormalForm): Option[Point] = {
    val A1 = l1.A
    val B1 = l1.B
    val C1 = l1.C
    val A2 = l2.A
    val B2 = l2.B
    val C2 = l2.C
    // Code below taken online
    val delta = A1 * B2 - A2 * B1
    if (delta == 0) None
    else {
      val x = (B2 * C1 - B1 * C2) / delta
      val y = (A1 * C2 - A2 * C1) / delta
      Some(Point(x, y))
    }
  }
}
case class LineSegmentNormalForm(A: Double, B: Double, C: Double) // Ax + By = C