case class Environment(items: Seq[Geo]) {
  def filter(dontInclude: Seq[Geo]): Seq[Geo] = {
    items.filterNot { case g: Geo => dontInclude.contains(g) }
  }
}



