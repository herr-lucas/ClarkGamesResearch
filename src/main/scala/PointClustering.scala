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

case class VisibilitySet(var visibilities: Seq[Visibility], color: String) {
  //def distance(v: Visibility): Double = {
  //  visibilities.map(vis => v.distance(vis)).max
  //}

  def distancePercentile(v: Visibility): Double = {
    val percentile = 0.1
    val tenthPercentileIndex = visibilities.size - Math.ceil(visibilities.size * percentile).toInt
    val sortedDistances = visibilities.map(vis => v.distance(vis)).sorted
    //println(s"Num items = ${visibilities.size} percentile index: $tenthPercentileIndex, distances $sortedDistances")
    sortedDistances(tenthPercentileIndex)
  }

  def setDistance(v: VisibilitySet): Double = {
    v.visibilities.map(visibility => distancePercentile(visibility)).max //).toOption.getOrElse(0)
  }

  def center: Visibility = {
    visibilities.map(v => (v, distancePercentile(v))).minBy(_._2)._1
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

    visibility.foreach { v =>
      val minGroup = clusters.minBy(_.distancePercentile(v))
      val index = clusters.indexOf(minGroup)
      if (verbose) {
        println(s"adding ${v.p} to ${clusters(index).color}")
      }
      clusters(index) = VisibilitySet(minGroup.visibilities :+ v, minGroup.color)
    }
    EnvironmentSegmentation(clusters, e)
  }

  def initialCluster(points: , size: Int) = {
    var visibility: Set[Visibility] = determineVisibility(points, e).visibility
    var clusters: MutableSeq[VisibilitySet] = MutableSeq.empty
    val colors = takeColors(size)
    (0 until size).foreach { i =>
      val (taken: Visibility, left: Set[Visibility]) = PointClustering.takeRandom(visibility)
      visibility = left
      clusters = clusters :+ VisibilitySet(Seq(taken), colors(i))
    }
    if (verbose) {
      val out = clusters.map(c => (c.color, c.visibilities.map(_.p)))
      println(s"cluster initialization points ${out}")
    }
    assert(clusters.size == size)
  }

  def repeatCluster(e: EnvironmentSegmentation): EnvironmentSegmentation = {
    val centers = e.partition.map(_.center)
  }
}


