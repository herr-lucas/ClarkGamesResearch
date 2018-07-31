import scala.util.Try
import scala.collection.mutable.{Seq => MutableSeq}
import scala.util.Random

case class Visibility(p: Point, visible: Set[Point]) {
  def distance(other: Visibility): Double = {
    val uniques = (other.visible.diff(this.visible) ++ this.visible.diff(other.visible))
    val dist = uniques.size * 1.0 / other.visible.union(this.visible).size
    assert (dist <= 1.0)
    dist
  }
}

case class VisibilitySet(var visibilities: Seq[Visibility]) {
  //def distance(v: Visibility): Double = {
  //  visibilities.map(vis => v.distance(vis)).max
  //}

  def distancePercentile(v: Visibility): Double = {
    val tenthPercentileIndex = visibilities.size - Math.ceil(visibilities.size / 10.0).toInt
    val sortedDistances = visibilities.map(vis => v.distance(vis)).sorted
    println(s"Tenth percentile index: $tenthPercentileIndex, distances: $sortedDistances")
    sortedDistances(tenthPercentileIndex)
  }

  def setDistance(v: VisibilitySet): Double = {
    v.visibilities.map(visibility => distancePercentile(visibility)).max //).toOption.getOrElse(0)
  }
}

case class EnvironmentSegmentation(partition: Seq[VisibilitySet], environment: Environment) {
  def getSet(p: Point) = {
    val res = partition.filter(_.visibilities.map(_.p).contains(p))
    assert(res.size == 1)
    res(0)
  }

  //def getNeighbors(visibilitySet: VisibilitySet): Seq[VisibilitySet]

  //def getBestNeighboringSet(p: Point) = {} // need to check for neighbor and visibility
}

case class PointClustering(visibility: Set[Visibility], environment: Environment)

object PointClustering {
  val colors = Seq("black", "green", "orange", "yellow", "grey", "blue", "pink", "red", "brown", "silver")
  def takeColors(num: Int): Seq[String] = colors.take(num)

  def takeRandom(visibility: Set[Visibility]): (Visibility, Set[Visibility]) = {
    val rIdx = Random.nextInt(visibility.size)
    val taken = visibility.toSeq.zipWithIndex.filter {
      case (v: Visibility, idx: Int) => idx == rIdx
    }.map(_._1)
    val remaining = visibility.take(rIdx) ++ visibility.drop(rIdx+1)
    assert(remaining.size == visibility.size - 1)
    assert(taken.size == 1)
    (taken(0), remaining)
  }

  def determineVisibility(points: Set[Point], environment: Environment): PointClustering = {
    val vis = points.map { p =>
      Visibility(p, points.flatMap(pcand => if (pcand.isVisible(p, environment)) Some(pcand) else None).toSet)
    }
    PointClustering(vis, environment)
  }

  def cluster(points: Set[Point], e: Environment, size: Int, verbose: Boolean = false): EnvironmentSegmentation = {
    var visibility: Set[Visibility] = determineVisibility(points, e).visibility
    var clusters: MutableSeq[VisibilitySet] = MutableSeq.empty
    (0 until size).foreach { i =>
      val (taken: Visibility, left: Set[Visibility]) = PointClustering.takeRandom(visibility)
      visibility = left
      clusters = clusters :+ VisibilitySet(Seq(taken))
    }
    if (verbose) {
      val out = clusters.map(_.visibilities.map(_.p))
      println(s"cluster initialization points ${out}")
    }
    assert(clusters.size == size)
    visibility.foreach { v =>
      val minGroup = clusters.minBy(_.distancePercentile(v))
      val index = clusters.indexOf(minGroup)
      if (verbose) {
        println(s"adding ${v.p} to ${clusters(index).visibilities.map(_.p)}")
      }
      clusters(index) = VisibilitySet(minGroup.visibilities :+ v)
    }
    EnvironmentSegmentation(clusters, e)
  }
}


