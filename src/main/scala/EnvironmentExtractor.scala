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
    loadCallOfDutyMap()
  }

  def extractLines(svg: xml.Elem): Seq[LineSegment] = {
    val lines = svg \\ "line"
    var ctr = 0
    lines.map { l =>
      {
        ctr += 1
        LineSegment(
          Point(x = (l \ "@x1").toString.toDouble, y = (l \ "@x2").toString.toDouble),
          Point(x = (l \ "@x2").toString.toDouble, y = (l \ "@y2").toString.toDouble),
          lid = Some(ctr)
        )
      }
    }
  }

  def loadCallOfDutyMap(): Seq[LineSegment] = {
    val xml = XML.loadFile("codvacant.svg")
    println(xml \\ "g" \\ "path")
    Seq.empty
  }
}



