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
    loadAndRenderCallOfDutyMap()
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
      val coords = s.split(" ")
      val filteredCoords = coords.filterNot(i => i == "m" || i == "l" | i == "c")
      val points: Seq[Point] = filteredCoords.map { s: String =>
        s.split( ",") match {
          case Array(x: String, y: String) => Point(x.toDouble, y.toDouble)
          case Array("z") => Point(0.0, 0.0)
        }
      }
      extractLinesFromPath(points)
    }
  }

  def extractLinesFromPath(pts: Seq[Point]): Seq[LineSegment] = {
    var currentPoint = Point(pts(0).x, pts(0).y)
    var lineSegments: Seq[LineSegment] = Seq.empty
    var j = 0;
    for (j <- 0 until pts.length - 1) {
      lineSegments = lineSegments :+ LineSegment(pts(j), pts(j + 1))
    }
    lineSegments
  }

  def loadSimpleBoxEnv(): Seq[LineSegment] = {
    val xml = XML.loadFile("environments/simpleBox.svg")
    val lines = extractLines(xml)
    lines
  }

  def loadAndRenderCallOfDutyMap(): Seq[LineSegment] = {
    val xml = XML.loadFile("environments/codvacant.svg")
    val paths = extractLinesFromPaths(xml).flatten
    EnvironmentRenderer.render(Environment(paths :+ Border(-1000, 1000, -1000, 1000)), fout = "environments/tests/cod_out.svg")
    paths
  }
}



