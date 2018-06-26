// TODO: IDS for geos should be in here not in the objects themselves.
case class Environment(items: Seq[Geo]) {
  def filter(dontInclude: Seq[Geo]): Seq[Geo] = {
    items.filterNot(i => dontInclude.map(_.id).contains(i.id))
  }
}
