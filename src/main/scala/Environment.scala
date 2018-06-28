// TODO: IDS for geos should be in here not in the objects themselves.
case class Environment(items: Seq[Geo]) {
  need to make sure assertion below works
  assert(items.map(_.id).toSet.size == items.size)
  def filter(dontInclude: Seq[Geo]): Seq[Geo] = { // TODO: should probably take ids not Geo's
    items.filterNot(i => dontInclude.map(_.id).contains(i.id))
  }
  /* TODO: below but unneccesary unless non-square environments?
  def isInside(p: Point): Boolean = {
    val verticalPInfinity = Point(-1, "", p.x, Double.MaxValue)
    (p.numIntersections(verticalPInfinity, ONLY include items that makeup outer box) % 2 == 1)
  }*/
}
