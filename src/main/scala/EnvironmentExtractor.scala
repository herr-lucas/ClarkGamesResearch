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
        ctr += 1
        LineSegment(
          Point(x = (l \ "@x1").toString.toDouble, y = (l \ "@y1").toString.toDouble),
          Point(x = (l \ "@x2").toString.toDouble, y = (l \ "@y2").toString.toDouble),
          lid = Some(ctr)
        )
      }
    }
  }

  def extractLinesFromPaths(svg: xml.NodeSeq): Seq[Seq[LineSegment]] = {
    val paths = svg \\ "path"
    val pathsStrings = paths.map { p =>
      (p \ "@d").toString()
    }
    pathsStrings.map { s: String =>
      val items = s.split(" ")
      val xyVals: Seq[String] = items match {
        case Array("m", _*) if items.endsWith("z") => items.takeRight(items.length - 1).take(items.length - 2) :+ items(1)
        case Array("m", _*) => items.takeRight(items.length - 1)
        case _ => { println("Path didn't start with m!!!"); Seq.empty }
      }
      val vals: Seq[(Double, Double)] = xyVals.map { s: String =>
        s.split( ",") match {
          case Array(x: String, y: String) => (x.toDouble, y.toDouble)
          case other => { println("Something went wrong with " + s); (0.0, 0.0) }
        }
      }
      var currentPointsX: Double = 0
      var currentPointsY: Double = 0
      var pts: Seq[TwoDVector] = Seq.empty // Replace this name with directions
      vals.foreach { p: (Double, Double) =>
        currentPointsX += p._1
        currentPointsY += p._2
        pts = pts :+ TwoDVector(currentPointsX, currentPointsY)
      }
      extractLinesFromPath(pts)
    }
  }

  def extractLinesFromPath(pts: Seq[TwoDVector]): Seq[LineSegment] = {
    var currentPoint = Point(pts(0).x, pts(0).y)
    var lineSegments = Seq.empty
    var j = 1;
    for (j <- 1 until pts.length) {
      val nextPoint = Point(currentPoint.x + pts(j).x, currentPoint.y + pts(j).y)
      lineSegments :+ LineSegment(currentPoint.copy(), nextPoint.copy())
    }
    lineSegments
  }

  def loadSimpleBoxEnv(): Seq[LineSegment] = {
    val xml = XML.loadFile("environments/simpleBox.svg")
    val lines = extractLines(xml)
    println("Simple box lines " + lines)
    lines
  }

  def loadCallOfDutyMap(): Seq[Seq[LineSegment]] = {
    val xml = XML.loadFile("environments/codvacant.svg")
    val paths = extractLinesFromPaths(xml)
    println(paths)
    paths
  }
}



