import scala.util.Try

case class VisibilitySet(var visibilities: Seq[Visibility]) {
  def distance(v: Visibility): Double = {
    visibilities.map(vis => v.distance(vis)).max
  }

  def setDistance(v: VisibilitySet): Double = {
    Try(v.visibilities.map(visibility => distance(visibility)).max).toOption.getOrElse(0)
  }
}

case class EnvironmentSegmentation(partition: Seq[VisibilitySet], environment: Environment)

case class Visibility(p: Point, visible: Set[Point]) {
  def distance(other: Visibility): Double = {
    other.visible.intersect(this.visible).size
  }
}

case class PointClustering(visibility: Set[Visibility], environment: Environment)

object PointClustering {
  def take(visibility: Set[Visibility], size: Int): (Seq[Visibility], Set[Visibility]) = {
    val taken = visibility.toSeq.take(size)
    val remaining = visibility.filterNot(taken.contains(_))
    (taken, remaining)
  }

  def determineVisibility(points: Set[Point], environment: Environment): PointClustering = {
    val vis = points.map { p =>
      Visibility(p, points.flatMap(pcand => if (pcand.isVisible(p, environment)) Some(pcand) else None).toSet)
    }
    PointClustering(vis, environment)
  }

  def cluster(points: Set[Point], e: Environment, size: Int): EnvironmentSegmentation = {
    var visibility: Set[Visibility] = determineVisibility(points, e).visibility
    var clusters: Seq[VisibilitySet] = Seq.empty
    (0 until size).foreach { i =>
      val (taken: Seq[Visibility], left: Set[Visibility]) = PointClustering.take(visibility, 1)
      visibility = left
      clusters :+ VisibilitySet(taken)
    }
    assert(clusters.size == size)
    visibility.foreach { v =>
      val minGroup = clusters.minBy(_.distance(v))
      minGroup.visibilities :+ v
    }
    EnvironmentSegmentation(clusters, e)
  }
}


