import MathHelpers._

case class LineSegment(lid: Int = -1, lname: String = "", p1: Point, p2: Point,
                       val numSamples: Int = 5, override val verbose: Boolean = false) extends Geo(lid, lname, verbose) {
  lazy val samples: Seq[Point] = pointSamples(numSamples)
  def isVisibleR(g: Geo, geos: Seq[Geo]): Boolean = {
    samples.map(_.isVisibleR(g, geos)).exists(_ == true)
  }

  def isVisible(g: Geo, e: Environment): Boolean = {
    val res = isVisibleR(g, e.filter(Seq(this, g)))
    res
  }

  def intersect(pointAndTwoDVector: PointAndTwoDVector): Boolean = {
    intersectPoint(pointAndTwoDVector).getOrElse(false)
  }

  def intersectPoint(pointAndTwoDVector: PointAndTwoDVector): Option[Point] = {
    val normalFormOther = pointAndTwoDVector.toLineSegmentNormalForm
    val normalForm = new PointAndTwoDVector(p1, p2).toLineSegmentNormalForm
    val intersect = LineSegmentNormalForm.lineIntersection(normalForm, normalFormOther)
    intersect.flatMap(x => if (contains(x)) Some(x) else None)
  }

  def contains(p: Point): Boolean = {
    val d1 = new PointAndTwoDVector(p1, p2).d
    val d2 = new PointAndTwoDVector(p, p2).d
    val diff = relativeError(d2.x  * d1.y, d2.y * d1.x)
    (diff < NumericalIntersectionError && sameSign(d1.x, d2.x) && sameSign(d1.y, d2.y))
  }

  def intersectDistance(pointAndTwoDVector: PointAndTwoDVector): Option[Double] = {
    val normalFormOther = pointAndTwoDVector.toLineSegmentNormalForm
    val normalForm = new PointAndTwoDVector(p1, p2).toLineSegmentNormalForm
    val intersect = LineSegmentNormalForm.lineIntersection(normalForm, normalFormOther)
    if (verbose) {
      println(s"Intersection point: $intersect")
    }
    if (intersect.map(contains(_)).getOrElse(false)) {
      intersect.map(_.dist(pointAndTwoDVector.p))
    } else None
  }

  def length(): Double = {
    p1.dist(p2)
  }

  def pointSamples(numSamples: Int): Seq[Point] = {
    assert(numSamples >= 2)
    (0 to numSamples - 1).map(x => stepSample(x * 1.0 / (numSamples - 1)))
    // TODO: Could also consider adding some random samples with (1 to 10).map(_ => randomPoint())
  }

  def stepSample(percentage: Double): Point = {
    assert(percentage >= 0 && percentage <= 1)
    val difference = p1.pathToPoint(p2)
    Point(0, "", p1.x + percentage * difference.x, p1.y + percentage * difference.y, verbose)
  }

  def randomPoint(): Point = {
    val difference = p1.pathToPoint(p2)
    val rand = Math.random()
    Point(0, "", p1.x + rand * difference.x, p1.y + rand * difference.y, verbose)
  }

  override def toString: String = if (this.id == -1) p1 + " to " + p2 else s"id $lid, name $lname $p1 to $p2"
}