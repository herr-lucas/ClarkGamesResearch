import java.lang.Math;

trait VisionTestable {
  def isVisible(g: Geo, e: Environment, verbose: Boolean): Boolean
  def intersect(pointAndDirection: PointAndDirection): Boolean
  def intersectDistance(pointAndDirection: PointAndDirection): Option[Double]
}
abstract class Geo(val id: Int, val name: String) extends VisionTestable
case class Point(pid: Int, pname: String, x: Double, y: Double) extends Geo(pid, pname) {
  def isVisible(g: Geo, e: Environment, verbose: Boolean = false): Boolean = {
    g match {
      case p: Point => {
        val closest_intersect: Seq[Double] = {
          println(e.items)
          e.items
            .filterNot(_.id == id)
            .filterNot(_.id == p.id)
            .map(_.intersectDistance(PointAndDirection(p, pathToPoint(p))))
        }.flatten
        if (closest_intersect.isEmpty) true
        else if (closest_intersect.min < dist(p)) {
          if (verbose)
            println("Blocked by: " + e.items
              .filterNot(_.id == id)
              .filterNot(_.id == p.id)
              .filterNot(_.intersectDistance(PointAndDirection(p, pathToPoint(p))) == None)
            )
          false
        }
        else true
      }
    }
  }
  def dist(p: Point): Double = {
    Math.sqrt(Math.pow(p.x - x, 2) + Math.pow((p.y - y), 2))
  }
  def pathToPoint(p: Point): Direction = {
    Direction(p.x - x, p.y - y)
  }
  def intersect(pointAndDirection: PointAndDirection): Boolean = {
    val dx = pointAndDirection.p.x - x
    val dy = pointAndDirection.p.y - y
    if (dy * pointAndDirection.d.x == dx * pointAndDirection.d.y) // Double check this logic
      true
    else
      false
  }
  def intersectDistance(pointAndDirection: PointAndDirection): Option[Double] = {
    val insersects = intersect(pointAndDirection)
    if (insersects) Some(dist(pointAndDirection.p))
    else None
  }
}

case class PointAndDirection(p: Point, d: Direction)
case class Direction(x: Double, y: Double)

case class Line(lid: Int, lname: String, p1: Point, p2: Point) extends Geo(lid, lname) {
  def isVisible(g: Geo, e: Environment, numSamples: Int = 100): Boolean = {
    g match {
      case p: Point => {
        pointSamples(numSamples).map(_.isVisible(g, e)).exists(_ == true)
      }
    }

    def intersect(pointAndDirection: PointAndDirection): Boolean = {

    }

    def intersectDistance(pointAndDirection: PointAndDirection): Option[Double] = {

    }
  }
  def pointSamples(numSamples: Int): Seq[Point] = {
    Seq(numSamples).map(_ => randomPoint())
  }
  def randomPoint(): Point = {
    val difference = p1.pathToPoint(p2)
    val max = p1.dist(p2)
    val rand = Math.random() * max
    Point(0, "", p1.x + rand * difference.x, p1.y + rand * difference.y)
  }
}
/*
case class Shape(lines: Seq[Line]) extends Geo {
  def isVisible(g: Geo, e: Environment): Boolean = false
}*/

case class Environment(items: Seq[Geo])

object Start {
  def main(args: Array[String]): Unit = {
    simple3PointIntersectionTest()
    simple3PointNoIntersectionTest()
  }

  def simple3PointIntersectionTest(): Unit = {
    val p1 = Point(0, "p1", 0, 0)
    val p2 = Point(1, "p2", 0, 5)
    val p3 = Point(2, "p3", 0, 10)
    val e = new Environment(Seq(p1, p2, p3))
    println("Simple 3 point intersection test: " + {
      if (p1.isVisible(p3, e, verbose = true) == false) "passes" else "fails"}
    )
  }

  def simple3PointNoIntersectionTest(): Unit = {
    val p1 = Point(0, "p1", 0, 0)
    val p2 = Point(1, "p2", 1, 5)
    val p3 = Point(2, "p3", 0, 10)
    val e = new Environment(Seq(p1, p2, p3))
    println("Simple 3 point no intersection test: " + {
      if (p1.isVisible(p3, e, verbose = true) == true) "passes" else "fails"}
    )
  }
}

