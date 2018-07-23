import java.beans.Visibility
case class PointClustering(visibility: Set[Visibility], environment: Environment)
}

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
    var visibility = determineVisibility(points, e).visibility
    var clusters: Seq[VisibilitySet] = Seq.empty
    val groups = (0 until size).foreach {
      val (taken: Seq[Visibility], left: Set[Visibility]) = PointClustering,take(visibility, 1)
    }
  }
}

class VisibilitySet(visibilities: Seq[Visibility]) {
  def distance(v: Visibility): Double = {
    visibilities.map(vis => v.distance(vis)).max
  }

  def setDistance(v: VisibilitySet): Double = {
    v.visibilities.map(visibility => distance(visibility)).max
  }
}

class EnvironmentSegmentation(partition: Seq[VisibilitySet], environment: Environment)

case class Visibility(p: Point, visible: Set[Point]) {
  def distance(other: Visibility): Double = {
    other.visible.intersect(this.visible).size
  }
}


