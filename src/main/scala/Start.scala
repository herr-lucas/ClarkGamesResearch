import java.lang.Math;

trait VisionTestable {
  def isVisible(g: Geo, e: Environment): Boolean
  def intersect(pointAndDirection: PointAndDirection): Option[Point]
  def intersectDistance(pointAndDirection: PointAndDirection): Double
}
abstract class Geo extends VisionTestable
case class Point(x: Int, y: Int) extends Geo {
  def isVisible(g: Geo, e: Environment): Boolean = {
    g match {
      case p: Point => {
        val closest_intersect: Double = {
          e.items.map(_.intersectDistance(PointAndDirection(p, pathToPoint(p)))).min //.flatMap(dist(_))
        }
        println("Closest Intersect" + closest_intersect)
        println("Actual distance" + dist(p))
        if (closest_intersect < dist(p)) false
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
  def intersect(pointAndDirection: PointAndDirection): Option[Point] = {
    val difference = Point(pointAndDirection.p.x - x, pointAndDirection.p.y - y)
    val xdist: Double = difference.x * 1.0 / pointAndDirection.d.x
    val ydist: Double = difference.y * 1.0 / pointAndDirection.d.y
    println(xdist, ydist)
    if (xdist == ydist) Some(Point(this.x, this.y))
    else None
  }
  def intersectDistance(pointAndDirection: PointAndDirection): Double = {
    val insersects = intersect(pointAndDirection)
    insersects.map(dist(_)).getOrElse(Double.MaxValue)
  }
}

case class PointAndDirection(p: Point, d: Direction)
case class Direction(x: Double, y: Double)

/*
case class Line(p1: Point, p2: Point) extends Geo {
  def isVisible(g: Geo, e: Environment): Boolean = false // to be implemented
}
case class Shape(lines: Seq[Line]) extends Geo {
  def isVisible(g: Geo, e: Environment): Boolean = false // to be implemented
}*/
case class Environment(items: Seq[Geo])

object Start {
  def main(args: Array[String]): Unit = {
    val p1 = Point(0, 0)
    val p2 = Point(0, 5)
    val p3 = Point(0, 10)
    val e = Environment(Seq(p1, p2, p3))
    println("Should be false: " + p1.isVisible(p3, e))
  }
}

