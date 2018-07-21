import scala.xml._
// ctrl shift alt s

object EnvironmentExtractor {
  def loadTests(): Unit = {
    val simpleText = <foo>
      <bar type="greet">hi</bar> <bar type="count">1</bar> <bar type="color">yellow</bar>
    </foo>
    println("Environment simple text extracted as xml: passes")
    val threeLineSystem = <svg height="210" width="500">
      |
      <line x1="0" y1="0" x2="100" y2="100" style="stroke:rgb(255,0,0);stroke-width:2"/>
      |
      <line x1="0" y1="0" x2="100" y2="50" style="stroke:rgb(255,0,0);stroke-width:2"/>
      |
      <line x1="0" y1="0" x2="100" y2="75" style="stroke:rgb(255,0,0);stroke-width:2"/>
      |</svg>
    println("Extracted three line system as xml: passes")
    println(extractLines(threeLineSystem))
    println("Extracted three line system into scala: see above.")
    println("Loading call of duty map")
    loadCallOfDutyMap()
    println("Loading simple box env")
    loadSimpleBoxEnv()
  }

  def extractLines(svg: xml.Elem): Seq[LineSegment] = {
    val lines = svg \\ "line"
    var ctr = 0
    lines.map { l =>
      {
        LineSegment(
          Point(x = (l \ "@x1").toString.toDouble, y = (l \ "@y1").toString.toDouble),
          Point(x = (l \ "@x2").toString.toDouble, y = (l \ "@y2").toString.toDouble)
        )
      }
    }
  }

  def extractLinesFromPaths(svg: xml.NodeSeq): Seq[Seq[LineSegment]] = {
    val paths = svg \\ "path"
    val pathsStrings = paths.map { p =>
      (p \ "@d").toString()
    }
    val extractedPaths = pathsStrings.map { s: String =>
      val coords = s.split(" ")
      val filteredCoords = coords.filterNot(i => i == "m" || i == "l" | i == "c")
      val doesReturn = filteredCoords.endsWith("z")
      val finalCoords = if (doesReturn) filteredCoords.take(filteredCoords.length - 1) else filteredCoords
      val points: (Seq[TwoDVector]) = filteredCoords.map { s: String =>
        val pos = s.split( ",")
        pos match {
          case Array (x: String, y: String) => TwoDVector(x.toDouble, y.toDouble)
          case _ => TwoDVector(0, 0)
        }
      }.filter(_ != TwoDVector(0, 0))
      val lines = extractLinesFromPath(points, doesReturn)
      lines
    }
    println(extractedPaths)
    extractedPaths
  }

  def extractLinesFromPath(pts: Seq[TwoDVector], doesReturn: Boolean): Seq[LineSegment] = {
    var currentPoint = Point(pts(0).x, pts(0).y)
    var lineSegments: Seq[LineSegment] = Seq.empty
    var j = 0;
    for (j <- 0 until pts.length - 1) {
      val nextPoint = Point(currentPoint.x + pts(j + 1).x, currentPoint.y + pts(j + 1).y)
      lineSegments = lineSegments :+ LineSegment(currentPoint, nextPoint)
      currentPoint = nextPoint
    }
    if (doesReturn) lineSegments :+ LineSegment(currentPoint, Point(pts(0).x, pts(0).y))
    lineSegments
  }

  def loadSimpleBoxEnv(): Seq[LineSegment] = {
    val xml = XML.loadFile("environments/simpleBox.svg")
    val lines = extractLines(xml)
    lines
  }

  def loadCallOfDutyMap(): Seq[LineSegment] = {
    val xml = XML.loadFile("environments/codvacant.svg")
    val paths = extractLinesFromPaths(xml).flatten
    paths
  }

  def loadSimplePathEnv(): Seq[LineSegment] = {
    val xml = XML.loadFile("environments/codtest.svg")
    val paths = extractLinesFromPaths(xml).flatten
    paths
  }
}



