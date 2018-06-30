case class Border(y1BoxLine: LineSegment, y2BoxLine: LineSegment, x1BoxLine: LineSegment, x2BoxLine: LineSegment) {
  def getCoordinates: (Point, Point) = {
    (Point(x = y1BoxLine.p1.x, y = y1BoxLine.p1.y), Point(x = x2BoxLine.p2.x, y = x2BoxLine.p2.y))
  }

  def this(x1: Double, x2: Double, y1: Double, y2: Double) = {
    val y1BoxLine = LineSegment(-1, "", Point(x = x1, y = y1), Point(x = x2, y = y1))
    val y2BoxLine = LineSegment(-1, "", Point(x = x1, y = y2), Point(x = x2, y = y2))
    val x1BoxLine = LineSegment(-1, "", Point(x = x1, y = y1), Point(x = x1, y = y2))
    val x2BoxLine = LineSegment(-1, "", Point(x = x2, y = y1), Point(x = x2, y = y2))
    this(y1BoxLine, y2BoxLine, x1BoxLine, x2BoxLine)
  }
}

