object BasicTests {
  def simple3PointIntersectionTest: Unit = {
    val v = false
    val p1 = Point(0, "p1", 0, 0, verbose = v)
    val p2 = Point(1, "p2", 0, 5, verbose = v)
    val p3 = Point(2, "p3", 0, 10, verbose = v)
    val e = new Environment(Seq(p1, p2, p3))
    println("Simple 3 point intersection test: " + {
      if (p1.isVisible(p3, e) == false) "passes" else "FAILS!!!"}
    )
  }

  def simple3PointNoIntersectionTest: Unit = {
    val p1 = Point(0, "p1", 0, 0)
    val p2 = Point(1, "p2", 1, 5)
    val p3 = Point(2, "p3", 0, 10)
    val e = new Environment(Seq(p1, p2, p3))
    println("Simple 3 point no intersection test: " + {
      if (p1.isVisible(p3, e) == true) "passes" else "FAILS!!!"}
    )
  }

  def simpleTwoLineSegmentIntersection: Unit = {
    val l1 = LineSegmentNormalForm(-1, 1, 5) // y = x + 5
    val l2 = LineSegmentNormalForm(-2, 1, 2) // y = 2x + 2
    val pIntersect = LineSegmentNormalForm.lineIntersection(l1, l2)
    println("Simple two line intersection test: "  + {
      if (pIntersect.get.x == 3.0 && pIntersect.get.y == 8.0) "passes" else "FAILS!!!"
    })
  }

  def simpleLineSegmentContainsTest: Unit = {
    val line = LineSegment(0 , "", Point(0, "", 0, 0), Point(0, "", 5, 10), 10)
    println("Simple line contains test: " + {
      if (line.contains(Point(x = 2.5, y =  5))) "passes" else "FAILS!!!"
    })
  }

  def simpleXMLLoadTest: Unit = {
    EnvironmentExtractor.loadTests()
  }

  def simpleThreeLineSegmentNoVisibilityTest = {
    val v = false
    val line1 = LineSegment(lid = 0, p1 = Point(x = 0, y = 0), p2 = Point(x = 0, y = 10), verbose = v) //vertical line
    val line2 = LineSegment(lid = 1, p1 = Point(x = 10, y = 0), p2 = Point(x = 10, y = 5), verbose = v) // verticla line 2
    val line3 = LineSegment(lid = 2, p1 = Point(x = 5, y = 0), p2 = Point(x = 5, y = 10), verbose = v) // vertical line 3
    val env = Environment(Seq(line1, line2, line3))
    println("Simple line blocking test  " + {if (line1.isVisible(line2, env)) "FAILS!!!" else "passes"}) // line 3 should block
  }

  def simpleThreeLineSegmentVisibilityTest: Unit = {
    val line1 = LineSegment(lid = 0, p1 = Point(x = 0, y = 0), p2 = Point(x = 0, y = 10)) // vertical line
    val line2 = LineSegment(lid = 1, p1 = Point(x = 10, y = 0), p2 = Point(x = 10, y = 5)) // vertical line 2
    val line3 = LineSegment(lid = 2, p1 = Point(x = 5, y = 0), p2 = Point(x = 5, y = 10)) // vertical line 3
    val env = Environment(Seq(line1, line2, line3))
    println("Simple line no block test " + {if (line1.isVisible(line3, env)) "passes" else "FAILS!!!"})
  }

  def complicatedThreeLineSegmentVisiblityTest: Unit = {
    val v = false
    val line1 = LineSegment(lid = 0, p1 = Point(x = 0, y = 0), p2 = Point(x = 10, y = 10), verbose = v) // vertical line
    val line2 = LineSegment(lid = 1, p1 = Point(x = 5, y = 0), p2 = Point(x = 8, y = 3), verbose = v) // vertical line 2
    val line3 = LineSegment(lid = 2, p1 = Point(x = 6, y = 0), p2 = Point(x = 10, y = 8), verbose = v) // vertical line 3
    val env = Environment(Seq(line1, line2, line3))
    println("Complex line no block test 1 " + {if (line1.isVisible(line2, env)) "passes" else "FAILS!!!"})
    println("Complex line no block test 2 " + {if (line1.isVisible(line3, env)) "passes" else "FAILS!!!"})
  }

  def complicatedThreeLineSegmentNoVisibilityTest: Unit = {
    val v = false
    val line1 = LineSegment(lid = 0, p1 = Point(x = 0, y = 0), p2 = Point(x = 10, y = 10), verbose = v) //vertical line
    val line2 = LineSegment(lid = 1, p1 = Point(x = 5, y = 0), p2 = Point(x = 10, y = 6), verbose = v) // verticla line 2
    val line3 = LineSegment(lid = 2, p1 = Point(x = 6, y = 0), p2 = Point(x = 8, y = 2.98), verbose = v) // vertical line 3
    val env = Environment(Seq(line1, line2, line3))
    println("Complex line block test 1 " + {if (!line1.isVisible(line3, env)) "passes" else "FAILS!!!"})
    println("Complex line block test 2 " + {if (!line3.isVisible(line1, env)) "passes" else "FAILS!!!"})
    println("Complex line block test 3 " + {if (line3.isVisible(line2, env)) "passes" else "FAILS!!!"})
    println("Complex line block test 4 " + {if (line1.isVisible(line2, env)) "passes" else "FAILS!!!"})
  }

  def simpleInternalPointTest: Unit = {
    val v = false
    val boxLine1 =  LineSegment(lid = 0, p1 = Point(x = 0, y = 0), p2 = Point(x = 0, y = 10), verbose = v) //vertical line
    val boxLine2 = LineSegment(lid = 1, p1 = Point(x = 0, y = 10), p2 = Point(x = 10, y = 10), verbose = v) //vertical line
    val boxLine3 = LineSegment(lid = 2, p1 = Point(x = 10, y = 10), p2 = Point(x = 10, y = 0), verbose = v) //vertical line
    val boxLine4 = LineSegment(lid = 3, p1 = Point(x = 10, y = 0), p2 = Point(x = 0, y = 0), verbose = v) //vertical line
    val pointInside1 = Point(pid = 4, "", 5, 5)
    val pointInside2 = Point(pid = 5, "", 3, 3)
    val lineInside = LineSegment(lid = 6, p1 = Point(x = 0, y = 0), p2 = Point(x = 4, y = 4), verbose = v) //vertical line
    val env = Environment(Seq(boxLine1, boxLine2, boxLine3, boxLine4, pointInside1, lineInside))
    // TODO: below unneccesary until we want to move to non-square environments, but why would we do that?
    // println(s"Point inside 1? ${if (env.isInside(pointInside1)) "passes" else "FAILS!!!"}")
    // println(s"Point inside 2? ${if (env.isInside(pointInside2)) "passes" else "FAILS!!!"}")

  }
}
