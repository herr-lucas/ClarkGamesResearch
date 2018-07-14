// TODO: IDS for geos should be in here not in the objects themselves.
// TODO: Need to run checks with borders not just items
case class Environment(items: Seq[Geo], border: Border) {
  //TODO: need to make sure assertion below works
  assert(items.map(_.id).filter(_ != None).distinct.size == items.map(_.id).filter(_ != None).size)
  def filter(dontInclude: Seq[Geo]): Seq[Geo] = { // TODO: should probably take ids not Geo's
    items.filterNot(i => dontInclude.map(_.id).contains(i.id))
  }
  /* TODO: below but unneccesary unless non-square regions?
  def isInside(p: Point): Boolean = {
    val verticalPInfinity = Point(-1, "", p.x, Double.MaxValue)
    (p.numIntersections(verticalPInfinity, ONLY include items that makeup outer box) % 2 == 1)
  }*/

  /*
  def boxSamples(x1: Double, x2: Double, y1: Double, y2: Double, numSamples: Int): EnvironmentPartition = {
    assert(x2 > x1)
    assert(y2 > y1)
    val points = (1 to numSamples).map { _ =>
      val x: Double = x1 + Math.random() * (x2 - x1)
      val y: Double = y1 + Math.random() * (y2 - y1)
      Point(x = x, y = y)
    }
    EnvironmentPartition(points, Border(x1, x2, y1, y2))

  }
  def boxSamples(b: Border, numSamples: Int): EnvironmentPartition = {
    val (p1, p2) = b.getCoordinates
    boxSamples(p1.x, p2.x, p1.y, p2.y, numSamples)
  }*/

  def partitionEnvironment(sliceSize: Int, numSamples: Int): Seq[EnvironmentPartition] = {
    val (bottomLeft: Point, topRight: Point) = border.getCoordinates
    val dx = topRight.x - bottomLeft.x
    val dy = topRight.y - bottomLeft.y
    assert(dx > 0)
    assert(dy > 0)
    (0 to sliceSize - 1).zip(0 to sliceSize - 1).map { // TODO: would sampling the edges be enough for boxes??
      case (x, y) => {
        val bottom_left_xC = bottomLeft.x + (dx * x * 1.0) / sliceSize
        val bottom_left_yC = bottomLeft.y + (dy * y * 1.0) / sliceSize
        val top_right_xC = bottomLeft.x + (dx * (x + 1) * 1.0) / sliceSize
        val top_right_yC = bottomLeft.y + (dy * (y + 1) * 1.0) / sliceSize
        val box = Border(x1 = bottom_left_xC, x2 = top_right_xC, y1 = bottom_left_yC, y2 = top_right_yC)
        EnvironmentPartition(box)
        //boxSamples(box, numSamples)
      }
    }
  }
}



