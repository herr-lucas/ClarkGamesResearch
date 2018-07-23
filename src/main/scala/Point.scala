import MathHelpers.{relativeError, sameSign, closeEnough}
import scala.util.Try

case class Point(x: Double, y: Double, override val verbose: Boolean = false, pname: Option[String] = None, override val specialColor: Option[String] = None) extends Geo(pname, verbose ) {
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
        if (verbose) println("Distances" + geos_and_distances.map(_._2))
        val dist_to_p = dist(p)
        if (verbose)
          println(s"dist $dist_to_p, $this " +
            s"$p min intersection ${Try(geos_and_distances.minBy(_._2)).toOption.map {
              case (g: Geo, dist: Double) => (g.intersectPoint(lineToPoint(p)), dist)
            }.getOrElse("no intersection")}"
          )
        if (geos_and_distances.isEmpty) true
        else if (geos_and_distances.map(_._2).min < dist_to_p) {
          false
        }
        else true
      }
      case l: LineSegment => {
        l.isVisibleR(this, geos)
      }
    }
  }

  def numIntersections(p: Point, geos: Seq[Geo]): Int = {
    val distances: Seq[Double] = {
      val pointAndVector = lineToPoint(p)
      geos.flatMap(g => g.intersectDistance(pointAndVector))
    }
    distances.length
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
    val dx = x - pointAndTwoDVector.p.x
    val dy = y - pointAndTwoDVector.p.y
    if (verbose) {
      println("Relative intersection distance:")
      println(relativeError(dy * pointAndTwoDVector.d.x,  dx * pointAndTwoDVector.d.y))
      println(s"Dx Dy $dx $dy")
      println(pointAndTwoDVector.d)
    }
    if (closeEnough(dy * pointAndTwoDVector.d.x, dx * pointAndTwoDVector.d.y)
      && sameSign(pointAndTwoDVector.d.x, dx) // make sure that they are the same sign
      && sameSign(pointAndTwoDVector.d.y, dy)) { // Double check this logic
      true
    }
    else {
      false
    }
  }

  def intersectPoint(pointAndTwoDVector: PointAndTwoDVector): Option[Point] = {
    if (intersect(pointAndTwoDVector)) Some(this) else None
  }

  def intersectDistance(pointAndTwoDVector: PointAndTwoDVector): Option[Double] = {
    val intersects = intersect(pointAndTwoDVector)
    if (intersects) Some(dist(pointAndTwoDVector.p))
    else None
  }

  override def toString: String = if (!name.isDefined) s"($x, $y)" else s"Name $name ($x, $y)"
}
