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
          e.filterIntersect(Seq(p, this)).map(_.intersectDistance(PointAndDirection(p, pathToPoint(p))))
        }.flatten
        if (closest_intersect.isEmpty) true
        else if (closest_intersect.min < dist(p)) {
          if (verbose)
            println("Blocked by: " + e.filterIntersect(Seq(p, this))
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

  override def toString: String = s"Id $id Name $name ($x, $y)"
}

object LineNormalForm {
  def lineIntersection(l1: LineNormalForm, l2: LineNormalForm): Option[Point] = {
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
case class LineNormalForm(A: Double, B: Double, C: Double) // Ax + By = C

case class PointAndDirection(p: Point, d: Direction) {
  def toLineNormalForm: LineNormalForm = {
    val A = -d.y
    val B = d.x
    val C = -d.y * p.x + p.y * d.x
    // sx*y = sy *(x - xi) + yi *sx
    LineNormalForm(A, B, C)
  }
}


case class Direction(x: Double, y: Double)

// Change this name to LineSegment?
case class Line(lid: Int, lname: String, p1: Point, p2: Point, val numSamples: Int) extends Geo(lid, lname) {
  lazy val samples = pointSamples(numSamples)
  def isVisible(g: Geo, e: Environment, verbose: Boolean = false): Boolean = {
    g match {
      case p: Point => {
        samples.map(_.isVisible(g, e)).exists(_ == true)
      }
    }
  }
  def intersect(pointAndDirection: PointAndDirection): Boolean = {
    false
  }

  def intersectDistance(pointAndDirection: PointAndDirection): Option[Double] = {
    Some(-1)
  }

  def length(): Double = {
    p1.dist(p2)
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

case class Environment(items: Seq[Geo]) {
  def filterIntersect(dontInclude: Seq[Geo]): Seq[Geo] = {
    items.filterNot(i => dontInclude.map(_.id).contains(i.id))
  }
}

object Start {
  def main(args: Array[String]): Unit = {
    simple3PointIntersectionTest()
    simple3PointNoIntersectionTest()
    simpleTwoLineIntersection()
    new EnvironmentExtractor
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

  def simpleTwoLineIntersection(): Unit = {
    val l1 = LineNormalForm(-1, 1, 5) // y = x + 5
    val l2 = LineNormalForm(-2, 1, 2) // y = 2x + 2
    val pIntersect = LineNormalForm.lineIntersection(l1, l2)
    println("Simple two line intersection test: "  + {
      if (pIntersect.get.x == 3.0 && pIntersect.get.y == 8.0) "passes" else "fails"
    })
  }
}

