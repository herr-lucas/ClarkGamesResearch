object BasicTests {
  def simple3PointIntersectionTest: Unit = {
    val p1 = Point(0, "p1", 0, 0)
    val p2 = Point(1, "p2", 0, 5)
    val p3 = Point(2, "p3", 0, 10)
    val e = new Environment(Seq(p1, p2, p3))
    println("Simple 3 point intersection test: " + {
      if (p1.isVisible(p3, e) == false) "passes" else "fails"}
    )
  }

  def simple3PointNoIntersectionTest: Unit = {
    val p1 = Point(0, "p1", 0, 0)
    val p2 = Point(1, "p2", 1, 5)
    val p3 = Point(2, "p3", 0, 10)
    val e = new Environment(Seq(p1, p2, p3))
    println("Simple 3 point no intersection test: " + {
      if (p1.isVisible(p3, e) == true) "passes" else "fails"}
    )
  }

  def simpleTwoLineSegmentIntersection: Unit = {
    val l1 = LineSegmentNormalForm(-1, 1, 5) // y = x + 5
    val l2 = LineSegmentNormalForm(-2, 1, 2) // y = 2x + 2
    val pIntersect = LineSegmentNormalForm.lineIntersection(l1, l2)
    println("Simple two line intersection test: "  + {
      if (pIntersect.get.x == 3.0 && pIntersect.get.y == 8.0) "passes" else "fails"
    })
  }

  def simpleLineSegmentContainsTest: Unit = {
    val line = LineSegment(0 , "", Point(0, "", 0, 0), Point(0, "", 5, 10), 10)
    println("Simple line contains test: " + {
      if (line.contains(Point(x = 2.5, y =  5))) "passes" else "fails"
    })
  }

  def simpleXMLLoadTest: Unit = {
    new EnvironmentExtractor
  }

  def simpleThreeLineSegmentVisibilityTest = {
    val line1 = LineSegment(p1 = Point(x = 0, y = 0), p2 = Point(x = 0, y = 10))
    val line2 = LineSegment(p1 = Point(x = 10, y = 0), p2 = Point(x = 10, y = 5))
    val line3 = LineSegment(p1 = Point(x = 5, y = 0), p2 = Point(x = 5, y = 1))
    val env = Environment(Seq(line1, line2, line3))
    println("Is line1 visible to line2? " + line1.isVisible(line3, env))
  }
}
