import MathHelpers._

case class LineSegment( p1: Point, p2: Point, val numSamples: Int = 5, override val verbose: Boolean = false,
                        lname: Option[String] = None) extends Geo(lname, verbose) {
  lazy val samples: Seq[Point] = pointSamples(numSamples)
  def isVisibleR(g: Geo, geos: Seq[Geo]): Boolean = {
    samples.map(_.isVisibleR(g, geos)).exists(_ == true)
  }

  def isVisible(g: Geo, e: Environment): Boolean = {
    val res = isVisibleR(g, e.filter(Seq(this, g)))
    res
  }

  def intersect(pointAndTwoDVector: PointAndTwoDVector): Boolean = {
    intersectPoint(pointAndTwoDVector).isDefined
  }

  def intersectPoint(pointAndTwoDVector: PointAndTwoDVector): Option[Point] = {
    val normalFormOther = pointAndTwoDVector.toLineSegmentNormalForm
    val normalForm = new PointAndTwoDVector(p1, p2).toLineSegmentNormalForm
    val intersect = LineSegmentNormalForm.lineIntersection(normalForm, normalFormOther)
    if (verbose) println("Is intersect " + intersect)
    val correctDirection: Boolean = intersect.map(p => {
      val path = pointAndTwoDVector.p.pathToPoint(p)
      sameSign(path.x, pointAndTwoDVector.d.x) && sameSign(path.y, pointAndTwoDVector.d.y)
    }).getOrElse(false)
    if (verbose) println("Correct direction " + correctDirection)
    if (verbose) println("Contains " + intersect.map((contains(_))))
    val result = intersect.flatMap(x => if (contains(x) && correctDirection) Some(x) else None)
    if (verbose) println(s"intersectResult $result")
    result
  }

  def contains(p: Point): Boolean = {
    val d1 = new PointAndTwoDVector(p1, p2).d
    val center = Point(0.5 * (p1.x + p2.x), 0.5 * (p1.y + p2.y))
    val d2 = new PointAndTwoDVector(p, p2).d
    val distClose = p.dist(center) <= 0.5 * p1.dist(p2)
    val sameVec = closeEnough(d2.x  * d1.y, d2.y * d1.x)
    if (verbose) println(s"d1 $d1 d2 $d2 sameVec  ${error(d2.x  * d1.y, d2.y * d1.x)}")
    val signX = sameSign(d1.x, d2.x)
    val signY = sameSign(d1.y, d2.y)
    if (verbose) println(s"distClose $distClose sameVec $sameVec signX $signX signY $signY")
    (distClose && sameVec && signX && signY)
  }

  def intersectDistance(pointAndTwoDVector: PointAndTwoDVector): Option[Double] = {
   intersectPoint(pointAndTwoDVector).map(_.dist(pointAndTwoDVector.p))
  }

  def length(): Double = {
    p1.dist(p2)
  }

  def pointSamples(numSamples: Int): Seq[Point] = {
    assert(numSamples >= 2)
    (0 to numSamples - 1).map(x => stepSample(x * 1.0 / (numSamples - 1)))
  }

  def stepSample(percentage: Double): Point = {
    assert(percentage >= 0 && percentage <= 1)
    val difference = p1.pathToPoint(p2)
    Point(p1.x + percentage * difference.x, p1.y + percentage * difference.y, verbose)
  }

  def randomPoint(): Point = {
    val difference = p1.pathToPoint(p2)
    val rand = Math.random()
    Point(p1.x + rand * difference.x, p1.y + rand * difference.y, verbose)
  }

  override def toString: String = if (!name.isDefined) p1 + " to " + p2 else s"name $lname $p1 to $p2"
}