// TODO: IDS for geos should be in here not in the objects themselves.
case class Environment(items: Seq[Geo], border: Border) {
  TODO: need to make sure assertion below works
  assert(items.map(_.id).distinct.size == items.size)
  def filter(dontInclude: Seq[Geo]): Seq[Geo] = { // TODO: should probably take ids not Geo's
    items.filterNot(i => dontInclude.map(_.id).contains(i.id))
  }
  /* TODO: below but unneccesary unless non-square regions?
  def isInside(p: Point): Boolean = {
    val verticalPInfinity = Point(-1, "", p.x, Double.MaxValue)
    (p.numIntersections(verticalPInfinity, ONLY include items that makeup outer box) % 2 == 1)
  }*/
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

  def paritionEnvironment(b: Border, sliceSize: Int): Seq[EnvironmentPartition] = {
    val (bottomLeft: Point, topRight: Point) = b.getCoordinates
    val dx = topRight.x - bottomLeft.x
    val dy = topRight.y - bottomLeft.y
    assert(dx > 0)
    assert(dy > 0)
    (0 to sliceSize - 1).zip(0 to sliceSize - 1).map {
      case (x, y) => {
        val bottom_left_xC = bottomLeft.x + (dx * x * 1.0) / sliceSize
        val bottom_left_yC = bottomLeft.y + (dy * y * 1.0) / sliceSize
        val top_right_xC = bottomLeft.x + (dx * (x + 1) * 1.0) / sliceSize
        val top_right_yC = bottomLeft.y + (dy * (y + 1) * 1.0) / sliceSize
        Border()
      }
    }
  }
}



