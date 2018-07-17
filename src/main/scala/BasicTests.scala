object BasicTests {
  // TODO: add ids to Borders?
  val numSamples = 10
  def simple3PointIntersectionTest: Unit = {
    val v = false
    val p1 = Point(0, 0, verbose = v)
    val p2 = Point(0, 5, verbose = v)
    val p3 = Point(0, 10, verbose = v)
    val e = new Environment(Seq(p1, p2, p3, Border(0, 100, 0, 100)))
    println("Simple 3 point intersection test: " + {
      if (p1.isVisible(p3, e) == false) "passes" else "FAILS!!!"}
    )
  }

  def simple3PointNoIntersectionTest: Unit = {
    val v = true
    val p1 = Point(0.1, 0.1, verbose = v)
    val p2 = Point(1, 5, verbose = v)
    val p3 = Point(0.1, 10, verbose = v)
    val e = new Environment(Seq(p1, p2, p3, Border(0, 100, 0, 100)))
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
    val line = LineSegment(Point(0, 0), Point(5, 10), numSamples = 10)
    println("Simple line contains test: " + {
      if (line.contains(Point(x = 2.5, y =  5))) "passes" else "FAILS!!!"
    })
  }

  def simpleXMLLoadTest: Unit = {
    EnvironmentExtractor.loadTests()
  }

  def simpleThreeLineSegmentNoVisibilityTest = {
    val v = false
    val line1 = LineSegment(Point(x = 0, y = 0), Point(x = 0, y = 10), verbose = v) //vertical line
    val line2 = LineSegment(Point(x = 10, y = 0), Point(x = 10, y = 5), verbose = v) // verticla line 2
    val line3 = LineSegment(Point(x = 5, y = 0), Point(x = 5, y = 10), verbose = v) // vertical line 3
    val env = Environment(Seq(line1, line2, line3, Border(0, 100, 0, 100)))
    println("Simple line blocking test  " + {if (line1.isVisible(line2, env)) "FAILS!!!" else "passes"}) // line 3 should block
  }

  def simpleThreeLineSegmentVisibilityTest: Unit = {
    val line1 = LineSegment(Point(x = 0, y = 0), Point(x = 0, y = 10)) // vertical line
    val line2 = LineSegment(Point(x = 10, y = 0), Point(x = 10, y = 5)) // vertical line 2
    val line3 = LineSegment(Point(x = 5, y = 0), Point(x = 5, y = 10)) // vertical line 3
    val env = Environment(Seq(line1, line2, line3, Border(0, 100, 0, 100)))
    println("Simple line no block test " + {if (line1.isVisible(line3, env)) "passes" else "FAILS!!!"})
  }

  def complicatedThreeLineSegmentVisiblityTest: Unit = {
    val v = false
    val line1 = LineSegment(Point(x = 0, y = 0), Point(x = 10, y = 10), verbose = v) // vertical line
    val line2 = LineSegment(Point(x = 5, y = 0), Point(x = 8, y = 3), verbose = v) // vertical line 2
    val line3 = LineSegment(Point(x = 6, y = 0), Point(x = 10, y = 8), verbose = v) // vertical line 3
    val env = Environment(Seq(line1, line2, line3, Border(0, 100, 0, 100)))
    println("Complex line no block test 1 " + {if (line1.isVisible(line2, env)) "passes" else "FAILS!!!"})
    println("Complex line no block test 2 " + {if (line1.isVisible(line3, env)) "passes" else "FAILS!!!"})
  }

  def complicatedThreeLineSegmentNoVisibilityTest: Unit = {
    val v = false
    val line1 = LineSegment(Point(0, 0), Point(10, 10), verbose = v) //vertical line
    val line2 = LineSegment(Point(5, 0), Point(10, 6), verbose = v) // verticla line 2
    val line3 = LineSegment(Point(6, 0), Point(8, 2.98), verbose = v) // vertical line 3
    val env = Environment(Seq(line1, line2, line3, Border(0, 100, 0, 100)))
    println("Complex line block test 1 " + {if (!line1.isVisible(line3, env)) "passes" else "FAILS!!!"})
    println("Complex line block test 2 " + {if (!line3.isVisible(line1, env)) "passes" else "FAILS!!!"})
    println("Complex line block test 3 " + {if (line3.isVisible(line2, env)) "passes" else "FAILS!!!"})
    println("Complex line block test 4 " + {if (line1.isVisible(line2, env)) "passes" else "FAILS!!!"})
  }

  def simpleInternalPointTest: Unit = {
    val v = false
    val boxLine1 =  LineSegment(Point(0, 0), Point(0, 10), verbose = v) //vertical line
    val boxLine2 = LineSegment(Point(0, 10), Point(10, 10), verbose = v) //vertical line
    val boxLine3 = LineSegment(Point(10, 10), Point(10, 0), verbose = v) //vertical line
    val boxLine4 = LineSegment(Point(10, 0), Point(0, 0), verbose = v) //vertical line
    val pointInside1 = Point(5, 5)
    val pointInside2 = Point(3, 3)
    val lineInside = LineSegment(Point(0, 0), Point(4, 4), verbose = v) //vertical line
    val env = Environment(Seq(boxLine1, boxLine2, boxLine3, boxLine4, pointInside1, lineInside, Border(0, 100, 0, 100)))
    // TODO: below unneccesary until we want to move to non-regions environments, but why would we do that?
    // println(s"Point inside 1? ${if (env.isInside(pointInside1)) "passes" else "FAILS!!!"}")
    // println(s"Point inside 2? ${if (env.isInside(pointInside2)) "passes" else "FAILS!!!"}")

  }

  def partitionEnvironment: Unit = {
    val v = false
    val pointInside1 = Point(5, 5)
    val pointInside2 = Point(3, 3)
    val env = Environment(items = Seq(pointInside1, pointInside2, Border(0, 100, 0, 100)))
    val partition = env.partitionEnvironment(10, numSamples, Border(0, 100, 0, 100))
    println("Test Partition: " + partition)
  }

  def testSimpleBoxEnv: Unit = {
    val lines = EnvironmentExtractor.loadSimpleBoxEnv()
    val point1 = Point(10, 10)
    val point2 = Point(25, 10)
    val point3 = Point(40, 10)
    val point4 = Point(10, 70)
    val point5 = Point(25, 70)
    val point6 = Point(40, 70)
    val env = Environment(lines :+ point1 :+ point2 :+ point3 :+ point4 :+ point5 :+ point6 :+ Border(0, 100, 0, 100))
    println("Test simple box env 1 " + {if (!point1.isVisible(point6, env)) "passes" else "FAILS!!!"})
    println("Test simple box env 2 " + {if (point1.isVisible(point4, env)) "passes" else "FAILS!!!"})
  }

  def renderEnvironment: Unit = {
    val point1 = Point(10, 10)
    val point2 = Point(25, 10)
    val point3 = Point(40, 10)
    val point4 = Point(10, 70)
    val point5 = Point(25, 70)
    val point6 = Point(40, 70)
    val lines = EnvironmentExtractor.loadSimpleBoxEnv()
    val env = Environment(lines :+ point1 :+ point2 :+ point3 :+ point4 :+ point5 :+ point6 :+ Border(0, 100, 0, 100))
    EnvironmentRenderer.render(env, "environments/tests/simpleBox.svg")
    println("Check environments/tests/simpleBox.svg to see if render worked!")
  }

  def testVisibilitySimpleBoxEnv(pFrom: Point) = {
    val (fromX: Double, fromY: Double) = (pFrom.x, pFrom.y)
    val lines = EnvironmentExtractor.loadSimpleBoxEnv()
    val pointOfInterest = Point(fromX, fromY, specialColor = Some("yellow"), verbose = false)
    val incrementSize = 4
    val scatterPoints =  (0 to 100 by incrementSize).flatMap(x => (0 to 100 by incrementSize).map(y => (x, y))).map {
      case (x: Int, y: Int) => {
        val color = if (Point(x ,y, verbose = true).isVisible(pointOfInterest, Environment(lines :+ Border(0, 100, 0, 100)))) "blue" else "red"
          Point(x, y, specialColor = Some(color))
      }
    }
    val env = Environment(lines ++ scatterPoints :+ pointOfInterest :+ Border(0, 100, 0, 100))
    EnvironmentRenderer.render(env, fout = s"environments/tests/boxVisibility$fromX-$fromY.svg")
  }

  def loadPaths: Unit = {
    val map = EnvironmentExtractor.loadAndRenderCallOfDutyMap()
    println("COD paths " + map)
    println("Printed the paths in the call of duty environment")
  }
}
