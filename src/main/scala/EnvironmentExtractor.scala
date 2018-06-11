import scala.xml._
// ctrl shift alt s

object EnvironmentExtractor {

  /*val TestFile = """
                   |<svg height="210" width="500">
                   |  <line x1="0" y1="0" x2="200" y2="200" style="stroke:rgb(255,0,0);stroke-width:2" />
                   |</svg>
                 """.stripMargin
  */
}
class EnvironmentExtractor {
  val foo = <foo><bar type="greet">hi</bar><bar type="count">1</bar><bar type="color">yellow</bar></foo>
  println("Environment extracted as xml: passes")
}


