import scala.util.Random

object BasicTests {
  val numSamples = 10

  def simple3PointIntersectionTest: Unit = {
    val v = false
    val p1 = Point(0, 0, verbose = v)
    val p2 = Point(0, 5, verbose = v)
    val p3 = Point(0, 10, verbose = v)
    val e = new Environment(Seq(p1, p2, p3, Border(0, 100, 0, 100)))
    println("Simple 3 point intersection test: " + {
      if (p1.isVisible(p3, e) == false) "passes" else "FAILS!!!"
    }
    )
  }

  def simple3PointNoIntersectionTest: Unit = {
    val v = false
    val p1 = Point(0.1, 0.1, verbose = v)
    val p2 = Point(1, 5, verbose = v)
    val p3 = Point(0.1, 10, verbose = v)
    val e = new Environment(Seq(p1, p2, p3, Border(0, 100, 0, 100)))
    println("Simple 3 point no intersection test: " + {
      if (p1.isVisible(p3, e) == true) "passes" else "FAILS!!!"
    }
    )
  }

  def simpleTwoLineSegmentIntersection: Unit = {
    val l1 = LineSegmentNormalForm(-1, 1, 5) // y = x + 5
    val l2 = LineSegmentNormalForm(-2, 1, 2) // y = 2x + 2
    val pIntersect = LineSegmentNormalForm.lineIntersection(l1, l2)
    println("Simple two line intersection test: " + {
      if (pIntersect.get.x == 3.0 && pIntersect.get.y == 8.0) "passes" else "FAILS!!!"
    })
  }

  def simpleLineSegmentContainsTest: Unit = {
    val line = LineSegment(Point(0, 0), Point(5, 10), numSamples = 10)
    println("Simple line contains test: " + {
      if (line.contains(Point(x = 2.5, y = 5))) "passes" else "FAILS!!!"
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
    println("Simple line blocking test  " + {
      if (line1.isVisible(line2, env)) "FAILS!!!" else "passes"
    })
  }

  def simpleThreeLineSegmentVisibilityTest: Unit = {
    val line1 = LineSegment(Point(x = 0, y = 0), Point(x = 0, y = 10)) // vertical line
    val line2 = LineSegment(Point(x = 10, y = 0), Point(x = 10, y = 5)) // vertical line 2
    val line3 = LineSegment(Point(x = 5, y = 0), Point(x = 5, y = 10)) // vertical line 3
    val env = Environment(Seq(line1, line2, line3, Border(0, 100, 0, 100)))
    println("Simple line no block test " + {
      if (line1.isVisible(line3, env)) "passes" else "FAILS!!!"
    })
  }

  def complicatedThreeLineSegmentVisiblityTest: Unit = {
    val v = false
    val line1 = LineSegment(Point(x = 0, y = 0), Point(x = 10, y = 10), verbose = v) // vertical line
    val line2 = LineSegment(Point(x = 5, y = 0), Point(x = 8, y = 3), verbose = v) // vertical line 2
    val line3 = LineSegment(Point(x = 6, y = 0), Point(x = 10, y = 8), verbose = v) // vertical line 3
    val env = Environment(Seq(line1, line2, line3, Border(0, 100, 0, 100)))
    println("Complex line no block test 1 " + {
      if (line1.isVisible(line2, env)) "passes" else "FAILS!!!"
    })
    println("Complex line no block test 2 " + {
      if (line1.isVisible(line3, env)) "passes" else "FAILS!!!"
    })
  }

  def complicatedThreeLineSegmentNoVisibilityTest: Unit = {
    val v = false
    val line1 = LineSegment(Point(0, 0), Point(10, 10), verbose = v) //vertical line
    val line2 = LineSegment(Point(5, 0), Point(10, 6), verbose = v) // verticla line 2
    val line3 = LineSegment(Point(6, 0), Point(8, 2.98), verbose = v) // vertical line 3
    val env = Environment(Seq(line1, line2, line3, Border(0, 100, 0, 100)))
    println("Complex line block test 1 " + {
      if (!line1.isVisible(line3, env)) "passes" else "FAILS!!!"
    })
    println("Complex line block test 2 " + {
      if (!line3.isVisible(line1, env)) "passes" else "FAILS!!!"
    })
    println("Complex line block test 3 " + {
      if (line3.isVisible(line2, env)) "passes" else "FAILS!!!"
    })
    println("Complex line block test 4 " + {
      if (line1.isVisible(line2, env)) "passes" else "FAILS!!!"
    })
  }

  def simpleInternalPointTest: Unit = {
    val v = false
    val boxLine1 = LineSegment(Point(0, 0), Point(0, 10), verbose = v) //vertical line
    val boxLine2 = LineSegment(Point(0, 10), Point(10, 10), verbose = v) //vertical line
    val boxLine3 = LineSegment(Point(10, 10), Point(10, 0), verbose = v) //vertical line
    val boxLine4 = LineSegment(Point(10, 0), Point(0, 0), verbose = v) //vertical line
    val pointInside1 = Point(5, 5)
    val pointInside2 = Point(3, 3)
    val lineInside = LineSegment(Point(0, 0), Point(4, 4), verbose = v) //vertical line
    val env = Environment(Seq(boxLine1, boxLine2, boxLine3, boxLine4, pointInside1, lineInside, Border(0, 100, 0, 100)))
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
    println("Test simple box env 1 " + {
      if (!point1.isVisible(point6, env)) "passes" else "FAILS!!!"
    })
    println("Test simple box env 2 " + {
      if (point1.isVisible(point4, env)) "passes" else "FAILS!!!"
    })
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
    EnvironmentRenderer.render(env, "environments/tests/simpleBox.svg", pointsAcross = Some(3), frameWidth = 100, frameHeight = 100)
    println("Check environments/tests/simpleBox.svg to see if render worked!")
  }

  def loadAndRenderSimplePath() = {
    val lines = EnvironmentExtractor.loadSimplePathEnv()
    val env = Environment(lines)
    EnvironmentRenderer.render(env, "environments/tests/simple_cod_path_test.svg", pointsAcross = None, frameWidth = 100, frameHeight = 100, verbose = false)
  }

  def testVisibilitySimpleBoxEnv(pFrom: Point) = {
    val (fromX: Double, fromY: Double) = (pFrom.x, pFrom.y)
    val lines = EnvironmentExtractor.loadSimpleBoxEnv()
    val pointOfInterest = Point(fromX, fromY, specialColor = Some("yellow"), verbose = false)
    val incrementSize = 4
    val scatterPoints = (0 to 100 by incrementSize).flatMap(x => (0 to 100 by incrementSize).map(y => (x, y))).map {
      case (x: Int, y: Int) => {
        val color = if (Point(x, y, verbose = false).isVisible(pointOfInterest, Environment(lines :+ Border(0, 100, 0, 100)))) "blue" else "red"
        Point(x, y, specialColor = Some(color))
      }
    }
    val env = Environment(lines ++ scatterPoints :+ pointOfInterest :+ Border(0, 100, 0, 100))
    EnvironmentRenderer.render(env, fout = s"environments/tests/boxVisibility$fromX-$fromY.svg", pointsAcross = Some(100 / incrementSize), frameWidth = 100, frameHeight = 100)
  }

  def temporaryCODDebuggingTests() = {
    val pfrom = Point(100, 100, verbose = true)
    val cands = Seq(Point(100, 390, verbose = true), Point(100, 400, verbose = true))
    val lines = EnvironmentExtractor.loadCallOfDutyMap()
    cands.foreach {
      cand: Point =>
        println(
          s"""$cand ${
            pfrom.isVisible(cand, Environment(Seq(LineSegment(Point(147.46686000000025, 258.6458500000001), Point(30.537730000000252, 258.6458500000001), verbose = true))))
          }""")
    }
  }

  def loadCallofDutyMap: Unit = {
    val map = EnvironmentExtractor.loadCallOfDutyMap()
    println("Loaded COD paths")
  }

  def renderCallOfDutyMap: Unit = {
    val map = EnvironmentExtractor.loadCallOfDutyMap()
    EnvironmentRenderer.render(Environment(map), "environments/tests/cod_out.svg", pointsAcross = None, frameWidth = 1000, frameHeight = 1000)
    println("Rendered COD paths")
  }

  def generatePoints(xEnd: Int, yEnd: Int, incrementSize: Int): Seq[Point] = {
    (0 to xEnd by incrementSize).flatMap(x => (0 to yEnd by incrementSize).map(y => (x, y))).map {
      case (x: Int, y: Int) => {
        Point(x, y)
      }
    }
  }

  def testCallOfDutyEnvironment(pFrom: Point) = {
    val (fromX: Double, fromY: Double) = (pFrom.x, pFrom.y)
    val lines = EnvironmentExtractor.loadCallOfDutyMap()
    val pointOfInterest = Point(fromX, fromY, specialColor = Some("yellow"), verbose = false)
    val incrementSize = 10
    val scatterPoints = (0 to 1000 by incrementSize).flatMap(x => (0 to 1000 by incrementSize).map(y => (x, y))).map {
      case (x: Int, y: Int) => {
        val color = if (Point(x, y, verbose = false).isVisible(pointOfInterest, Environment(lines))) "blue" else "red"
        Point(x, y, specialColor = Some(color))
      }
    }
    val env = Environment(lines ++ scatterPoints :+ pointOfInterest)
    EnvironmentRenderer.render(env, fout = s"environments/tests/callOfDutyVisibility$fromX-$fromY.svg", pointsAcross = Some(1000 / incrementSize), frameWidth = 1000, frameHeight = 1000)
  }

  def basicVisibilityTest(verbose: Boolean = false): Unit = {
    val print = verbose
    val points = Seq( Point(0, 0), Point(10, 0), Point(0, 10), Point(10, 10))
    val objects = Seq( LineSegment(Point(-10, 5), Point(15, 5)) )
    val env = Environment(objects)
    val pointClustering = PointClustering.determineVisibility( points.toSet, env )
    if (print) {
      println("Basic visiblity test")
      println("What (0, 0) can see " + pointClustering.visibility.filter(_.p == Point(0, 0)))
      println("What (10, 0) can see " + pointClustering.visibility.filter(_.p == Point(10, 0)))
      println("What (0, 10) can see " + pointClustering.visibility.filter(_.p == Point(0, 10)))
      println("What (10, 10) can see " + pointClustering.visibility.filter(_.p == Point(10, 10)))
    }
  }

  def basicSegmentationTest(verbose: Boolean = false): Unit = {
    val print = verbose
    val points = Seq(
      Point(0, 0), Point(5, 0), Point(10, 0),
      Point(0, 10), Point(5, 10), Point(10, 10)
    )
    val objects = Seq(LineSegment(Point(-10, 5), Point(15, 5)) )
    val env = Environment(objects)
    if (print) {
      println("Basic segmentation test")
    }
    val pointClustering = PointClustering.clusterRounds(points.toSet, env, size = 3, rounds = 1, verbose = verbose)
    if (print) println("Partition " + pointClustering.partition.map(_.visibilities.map(_.p)))
  }

  def distanceTest(verbose: Boolean = false) = {
    val points = Seq(
      Point(0, 0), Point(5, 0), Point(10, 0),
      Point(0, 10), Point(5, 10), Point(10, 10)
    )
    val objects = Seq(LineSegment(Point(-10, 5), Point(15, 5)))
    val env = Environment(objects)
    val visibilities = PointClustering.determineVisibility(points.toSet, env).visibility
    val visSet1 = VisibilitySet(visibilities.filter(x => x.p == Point(0, 0) || x.p == Point(5, 0)).toSeq, "green")
    val visSet2 = VisibilitySet(visibilities.filter(x => x.p == Point(0, 10) || x.p == Point(5, 10)).toSeq, "blue")
    val visx0y0 = visibilities.filter(x => x.p == Point(0, 0)).toSeq(0)
    val visx0y10 = visibilities.filter(x => x.p == Point(0, 10)).toSeq(0)
    val dist1 = visx0y0.distance(visx0y10)
    val distSet = visSet1.setDistance(visSet2)
    if (verbose) {
      println("Distance between visibilities of 0-0 and 0-10: " + dist1)
      println("Distances between sets: " + distSet)
    }
  }


  // Tests below generate segmentation output in environments/tests
  // Tests are sometimes run with different seeds (to generate segmentation froms different initializations)
  def environmentSegmentationTest(verbose: Boolean = false) = {
    val clus_size = 3
    val num_rounds = 3
    val lines = EnvironmentExtractor.loadSimpleBoxEnv()
    val env = Environment(lines)
    val segmentations = PointClustering.clusterRoundsSeq(generatePoints(100, 100, 10).toSet, env, rounds = num_rounds, size = clus_size, verbose = verbose)
    segmentations.zipWithIndex.foreach { case (segmentation, roundNumber) =>
      val colors = PointClustering.takeColors(clus_size)
      val coloredPoints = segmentation.partition.zip(colors).map {
        case (vis, color: String) => vis.visibilities.map(v => Point(v.p.x, v.p.y, specialColor = Some(color)))
      }.flatten
      EnvironmentRenderer.render(Environment(lines ++ coloredPoints), fout = s"environments/tests/boxEnvironmentSegmented-r$roundNumber-s.svg", pointsAcross = Some(10), frameWidth = 100, frameHeight = 100)
    }
  }

  def environmentSegmentationTestBestSeed(verbose: Boolean = false) = {
    val seeds = Seq(1, 2, 3, 4, 5)
    val clus_size = 3
    val num_rounds = 3
    val lines = EnvironmentExtractor.loadSimpleBoxEnv()
    val env = Environment(lines)
    val segmentations = seeds.map { s =>
      Random.setSeed(s)
      val segmentation = PointClustering.clusterRounds(generatePoints(100, 100, 10).toSet, env, rounds = num_rounds, size = clus_size, verbose = verbose)
      val colors = PointClustering.takeColors(clus_size)
      val coloredPoints = segmentation.partition.zip(colors).map {
        case (vis, color: String) => vis.visibilities.map(v => Point(v.p.x, v.p.y, specialColor = Some(color)))
      }.flatten
      (segmentation, coloredPoints)
    }
    if (verbose) println(s"Segmentation scores: ${segmentations.map(_._1.score)}")
    val bestSegmentation = segmentations.map(s => (s, s._1.score)).minBy(_._2)._1
    EnvironmentRenderer.render(Environment(lines ++ bestSegmentation._2), fout = s"environments/tests/boxEnvironmentSegmented_best_seed.svg", pointsAcross = Some(10), frameWidth = 100, frameHeight = 100)
  }

  def environmentSegmentationCODTest(verbose: Boolean = false) = {
    val clus_size = 10
    val num_rounds = 4
    val lines = EnvironmentExtractor.loadCallOfDutyMap()
    val env = Environment(lines)
    val pointsAcross = 40
    val segmentations = PointClustering.clusterRoundsSeq(generatePoints(1000, 1000, 1000/ pointsAcross).toSet, env, rounds = num_rounds, size = clus_size, verbose = verbose)
    segmentations.zipWithIndex.foreach { case (segmentation, roundNumber) =>
      val colors = PointClustering.takeColors(clus_size)
      val coloredPoints = segmentation.partition.zip(colors).map {
        case (vis, color: String) => vis.visibilities.map(v => Point(v.p.x, v.p.y, specialColor = Some(color)))
      }.flatten
      EnvironmentRenderer.render(Environment(lines ++ coloredPoints), fout = s"environments/tests/callOfDutySegmented-percentile-dist-$roundNumber.svg", pointsAcross = Some(10), frameWidth = 1000, frameHeight = 1000)
    }
  }
}