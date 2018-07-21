import java.io._

object EnvironmentRenderer {
  def render(environment: Environment, fout: String, frameWidth: Int, frameHeight: Int, verbose: Boolean = false): Unit = {
    val output = {
      s"""<svg height=\"${frameWidth}\" width=\"$frameHeight\">""" + environment.items.map { item: Geo =>
        if (verbose) println(s"rendering $item")
        item match {
          case l: LineSegment => {
            s"""<line x1="${l.p1.x}" y1="${l.p1.y}" x2="${l.p2.x}" y2="${l.p2.y}" style="stroke:rgb(255,0,0);stroke-width:2"/>"""
          }
          case p: Point => {
            s"""<circle cx="${p.x}" cy="${p.y}" r="0.5" stroke="${p.specialColor.getOrElse("black")}" stroke-width="3" fill="${p.specialColor.getOrElse("black")}"/>"""
          }
          case b: Border => {
            val x = b.x1BoxLine.p1.x
            val y = b.x1BoxLine.p1.y
            val width = b.x2BoxLine.p1.x - x
            val height = b.x1BoxLine.p2.y - b.x1BoxLine.p1.y
            s"""<rect x="${x}" y="${y}" width="${width}" height="${height}" style="fill:rgb(0,0,255);stroke-width:3;stroke:rgb(0,0,0)" fill-opacity="0.0"/>"""
          }
        }
      }.mkString("\n") + "</svg>"
    }
    val file = new File(fout)
    val bw = new BufferedWriter(new FileWriter(file))
    bw.write(output)
    bw.close()
  }

  def renderCallOfDutyMap(lines: Seq[LineSegment]): Unit = {
    EnvironmentRenderer.render(Environment(lines), frameWidth = 1000, frameHeight = 1000, fout = "environments/tests/cod_out.svg")
  }
}
