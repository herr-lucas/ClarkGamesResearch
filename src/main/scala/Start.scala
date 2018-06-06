import java.lang.Math;

trait VisionTestable {
  def isVisible(g: Geo, e: Environment, verbose: Boolean): Boolean
  def intersect(pointAndTwoDVector: PointAndTwoDVector): Boolean
  def intersectDistance(pointAndTwoDVector: PointAndTwoDVector): Option[Double]
}
abstract class Geo(val id: Int, val name: String) extends VisionTestable
case class Point(pid: Int = -1, pname: String = "", x: Double, y: Double) extends Geo(pid, pname) {
  def isVisible(g: Geo, e: Environment, verbose: Boolean = false): Boolean = {
    g match {
      case p: Point => {
        val closest_intersect: Seq[Double] = {
          println(e.filter(Seq(p, this))) // need to clean up ids here to make sure that proper lines are removed!
          e.filter(Seq(p, this)).map(_.intersectDistance(PointAndTwoDVector(p, pathToPoint(p))))
        }.flatten
        println(closest_intersect)
        if (closest_intersect.isEmpty) true
        else if (closest_intersect.min < dist(p)) {
          if (verbose)
            println("Blocked by: " + e.filter(Seq(p, this))
              .filterNot(_.intersectDistance(PointAndTwoDVector(p, pathToPoint(p))) == None)
            )
          false
        }
        else true
      }
      case l: LineSegment => {
        l.isVisible(this, e)
      }
    }
  }
  def dist(p: Point): Double = {
    Math.sqrt(Math.pow(p.x - x, 2) + Math.pow((p.y - y), 2))
  }

  def pathToPoint(p: Point): TwoDVector = {
    TwoDVector(p.x - x, p.y - y)
  }
  def intersect(pointAndTwoDVector: PointAndTwoDVector): Boolean = {
    val dx = pointAndTwoDVector.p.x - x
    val dy = pointAndTwoDVector.p.y - y
    println("V" + dy * pointAndTwoDVector.d.x)
    println("D" + (dy * pointAndTwoDVector.d.x -  dx * pointAndTwoDVector.d.y))
    if (dy * pointAndTwoDVector.d.x == dx * pointAndTwoDVector.d.y) // Double check this logic
      true
    else
      false
  }
  def intersectDistance(pointAndTwoDVector: PointAndTwoDVector): Option[Double] = {
    val insersects = intersect(pointAndTwoDVector)
    if (insersects) Some(dist(pointAndTwoDVector.p))
    else None
  }

  override def toString: String = s"Id $id Name $name ($x, $y)"
}

object LineSegmentNormalForm {
  def lineIntersection(l1: LineSegmentNormalForm, l2: LineSegmentNormalForm): Option[Point] = {
    val A1 = l1.A
    val B1 = l1.B
    val C1 = l1.C
    val A2 = l2.A
    val B2 = l2.B
    val C2 = l2.C
    // Code below taken from online
    val delta = A1 * B2 - A2 * B1
    if (delta == 0) None
    else {
      val x = (B2 * C1 - B1 * C2) / delta
      val y = (A1 * C2 - A2 * C1) / delta
      Some(Point(0, "", x, y))
    }
  }
}
case class LineSegmentNormalForm(A: Double, B: Double, C: Double) // Ax + By = C

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

case class LineSegment(lid: Int = -1, lname: String = "", p1: Point, p2: Point, val numSamples: Int = 3) extends Geo(lid, lname) {
  lazy val samples: Seq[Point] = pointSamples(numSamples)
  def isVisible(g: Geo, e: Environment, verbose: Boolean = false): Boolean = {
    samples.map(_.isVisible(g, e)).exists(_ == true)
  }

  def intersect(pointAndTwoDVector: PointAndTwoDVector): Boolean = {
    val normalFormOther = pointAndTwoDVector.toLineSegmentNormalForm
    val normalForm = new PointAndTwoDVector(p1, p2).toLineSegmentNormalForm
    val intersect = LineSegmentNormalForm.lineIntersection(normalForm, normalFormOther)
    intersect.map(contains(_)).getOrElse(false)
  }

  def contains(p: Point): Boolean = {
    val d1 = new PointAndTwoDVector(p1, p2).d
    val d2 = new PointAndTwoDVector(p, p2).d
    if (d1.x == 0) {
      if (d2.x == 0) {
        (d1.y > 0 & d2.y > 0 & d1.y > d2.y) || (d1.y < 0 & d2.y < 0 & d1.y < d2.y)
      } else false
    }
    else {
      val ratio  = d2.x / d1.x
      (ratio * d1.y == d2.y)
    }
  }

  def intersectDistance(pointAndTwoDVector: PointAndTwoDVector): Option[Double] = {
    Some(-1)
  }

  def length(): Double = {
    p1.dist(p2)
  }

  def pointSamples(numSamples: Int): Seq[Point] = {
    (1 to numSamples).map(_ => randomPoint())
  }
  def randomPoint(): Point = {
    val difference = p1.pathToPoint(p2)
    val rand = Math.random()
    Point(0, "", p1.x + rand * difference.x, p1.y + rand * difference.y)
  }
}

/*
case class Shape(lines: Seq[LineSegment]) extends Geo {
  def isVisible(g: Geo, e: Environment): Boolean = false
}*/

case class Environment(items: Seq[Geo]) {
  def filter(dontInclude: Seq[Geo]): Seq[Geo] = {
    items.filterNot(i => dontInclude.map(_.id).contains(i.id))
  }
}

object Start {
  import BasicTests._
  def main(args: Array[String]): Unit = {
    simple3PointIntersectionTest
    simple3PointNoIntersectionTest
    simpleTwoLineSegmentIntersection
    simpleXMLLoadTest
    simpleLineSegmentContainsTest
    simpleThreeLineSegmentVisibilityTest
  }
}

