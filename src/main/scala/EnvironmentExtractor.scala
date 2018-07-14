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

  def extractLinesFromPaths(xml: xml.Elem): Seq[LineSegment] = {
    val paths = xml \\ "path"
    paths.map { p =>
      (p \ "@d").toString()
    }.map { s: String =>
      val items = s.split(" ")
      val xyVals = items match {
        case List("m", _*, "z") => items.slice(1, items.length - 1) ++ items(1)
        case List("m", _*) => items.slice(1, items.length)
      }

      val vals = xyVals.map { s: String =>
        s.split(",") match {
          case List(x: String, y: String) => (x.toDouble, y.toDouble)
        }
      }
      var currentPointsX = 0
      var currentPointsY = 0
      var pts: Seq[Point] = Seq.empty
      vals.foreach { p: (Double, Double) =>
        currentPointsX += p._1
        currentPointsY += p._2
        pts = pts ++ Point(currentPointsX, currentPointsY)
      }
    }
  }

  def extractLinesFromPath() = {

  }

  def loadSimpleBoxEnv(): Seq[LineSegment] = {
    val xml = XML.loadFile("environments/simpleBox.svg")
    val lines = extractLines(xml)
    println("Simple box lines " + lines)
    lines
  }

  def loadCallOfDutyMap(): Seq[LineSegment] = {
    val xml = XML.loadFile("environments/codvacant.svg")
    println(xml \\ "g" \\ "path")
    Seq.empty
  }
}



