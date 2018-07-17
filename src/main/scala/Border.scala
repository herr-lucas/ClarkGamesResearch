import scala.util.Try
object Border {
  def apply(x1: Double, x2: Double, y1: Double, y2: Double): Border = {
    val y1BoxLine = LineSegment(Point(x = x1, y = y1), Point(x = x2, y = y1))
    val y2BoxLine = LineSegment(Point(x = x1, y = y2), Point(x = x2, y = y2))
    val x1BoxLine = LineSegment(Point(x = x1, y = y1), Point(x = x1, y = y2))
    val x2BoxLine = LineSegment(Point(x = x2, y = y1), Point(x = x2, y = y2))
    this(y1BoxLine, y2BoxLine, x1BoxLine, x2BoxLine)
  }
}
case class Border(y1BoxLine: LineSegment, y2BoxLine: LineSegment, x1BoxLine: LineSegment, x2BoxLine: LineSegment, bid: Option[Int] = None, bname: Option[String] = None)
  extends Geo(id = bid, name = bname, verbose = false) {
  val lines: Seq[LineSegment] = Seq(y1BoxLine, y2BoxLine, x1BoxLine, x2BoxLine)
  val points = lines.flatMap(l => l.samples)
  def getCoordinates: (Point, Point) = {
    (Point(x = y1BoxLine.p1.x, y = y1BoxLine.p1.y), Point(x = x2BoxLine.p2.x, y = x2BoxLine.p2.y))
  }

  def isLineVisible(l: LineSegment, geos: Seq[Geo]): Boolean = {
    points.exists {
      p: Point => l.isVisibleR(p, geos)
    }
  }

  override def intersect(pointAndTwoDVector: PointAndTwoDVector): Boolean = {
    intersectDistance(pointAndTwoDVector).isDefined
  }

  override def intersectPoint(pointAndTwoDVector: PointAndTwoDVector): Option[Point] = {
    Try(lines.map(l => l.intersectPoint(pointAndTwoDVector)).flatten.minBy(pintersect => pointAndTwoDVector.p.dist(pintersect))).toOption
  }

  override def intersectDistance(pointAndTwoDVector: PointAndTwoDVector): Option[Double] = {
    val distances = lines.map(l => l.intersectPoint(pointAndTwoDVector)).flatten.map(pintersect => pointAndTwoDVector.p.dist(pintersect))
    Try(distances.min).toOption
  }

  override def isVisible(g: Geo, env: Environment): Boolean = {
    g match {
      case p: Point => points.map(pBorder => pBorder.isVisibleR(p, env.filter(Seq(this, g)))).exists(_ == true)
      case l: LineSegment => isLineVisible(l, env.filter(Seq(g, this)))
      case b: Border => b.lines.map(l => isLineVisible(l, env.filter(Seq(g, this)))).exists(_ == true)
    }
  }
}

