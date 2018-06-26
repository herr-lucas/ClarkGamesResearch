import MathHelpers.{relativeError, sameSign}

case class Point(pid: Int = -1, pname: String = "", x: Double, y: Double, override val verbose: Boolean = false) extends Geo(pid, pname, verbose) {
  def isVisibleR(g: Geo, geos: Seq[Geo]): Boolean = {
    g match {
      case p: Point => {
        val geos_and_distances: Seq[(Geo, Double)] = {
          if (verbose) {
            println(s"Checking if $this can see $p. Candidate blockers are: $geos")
          }
          val pointAndVector = lineToPoint(p)
          if (verbose) println(s"Point And Vector $pointAndVector")
          geos.flatMap(g => g.intersectDistance(pointAndVector).map(x => (g, x)))
        }
        if (verbose) println(s"intersect distances ${geos_and_distances.map(_._2)}")
        val dist_to_p = dist(p)
        if (geos_and_distances.isEmpty) true
        else if (geos_and_distances.map(_._2).min < dist_to_p) {
          if (verbose) {
            println("Blocked by: " + geos_and_distances
              .filter(_._2 < dist_to_p)
            )
          }
          false
        }
        else true
      }
      case l: LineSegment => {
        l.isVisibleR(this, geos)
      }
    }
  }

  def lineToPoint(point: Point): PointAndTwoDVector = {
    PointAndTwoDVector(this, pathToPoint(point))
  }
  def isVisible(g: Geo, e: Environment): Boolean = {
    isVisibleR(g, e.filter(Seq(g, this)))
  }
  def dist(p: Point): Double = {
    Math.sqrt(Math.pow(p.x - x, 2) + Math.pow((p.y - y), 2))
  }

  def pathToPoint(p: Point): TwoDVector = {
    TwoDVector(p.x - x, p.y - y)
  }
  def intersect(pointAndTwoDVector: PointAndTwoDVector): Boolean = {
    // TODO: THIS desperatly needs to be fixed. !!
    val dx = x - pointAndTwoDVector.p.x
    val dy = y - pointAndTwoDVector.p.y
    if (verbose) {
      println("Relative intersection distance:")
      println(relativeError(dy * pointAndTwoDVector.d.x,  dx * pointAndTwoDVector.d.y))
      println(s"Dx Dy $dx $dy")
      println(pointAndTwoDVector.d)
    }
    if (dy * pointAndTwoDVector.d.x == dx * pointAndTwoDVector.d.y
      && sameSign(pointAndTwoDVector.d.x, dx) // make sure that they are the same sign
      && sameSign(pointAndTwoDVector.d.y, dy)) { // Double check this logic
      true
    }
    else {
      false
    }
  }
  def intersectDistance(pointAndTwoDVector: PointAndTwoDVector): Option[Double] = {
    val insersects = intersect(pointAndTwoDVector)
    if (insersects) Some(dist(pointAndTwoDVector.p))
    else None
  }

  override def toString: String = if (id == -1) s"($x, $y)" else s"Id $id Name $name ($x, $y)"
}
