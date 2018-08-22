import scala.util.Try
import scala.collection.mutable.{Seq => MutableSeq}
import scala.util.Random

case class Visibility(p: Point, visible: Set[Point]) {
  def distance(other: Visibility): Double = {
    val uniques = (other.visible.diff(this.visible) ++ this.visible.diff(other.visible))
    val totalVisible = other.visible.union(this.visible)
    val dist = uniques.size * 1.0 / totalVisible.size
    if ((p.x == 0.0 && p.y == 40.0) || (other.p.x == 0.0 && other.p.y == 40.0)) {
      //println(s"(UNIQUES, TOTAL, OTHER, THIS) (${uniques.size}, ${totalVisible.size}) ${other.visible.size} ${this.visible.size} ${this.p} ${other.p}")
    }
    assert (dist <= 1.0)
    dist
  }
}

case class VisibilitySet(var visibilities: Seq[Visibility], color: String) {
  def distance(v: Visibility): Double = distanceAVG(v)

  def distanceAVG(v: Visibility): Double = {
    visibilities.map(vis => v.distance(vis)).sum / visibilities.size
  }

  // These produce some weird behavior on cerain points
  /*def distanceWorst(v: Visibility): Double = {
    visibilities.map(vis => v.distance(vis)).max
  }


  def distancePercentile(v: Visibility): Double = {
    val percentile = 0.1
    val tenthPercentileIndex = visibilities.size - Math.ceil(visibilities.size * percentile).toInt
    val sortedDistances = visibilities.map(vis => v.distance(vis)).sorted
    //println(s"Num items = ${visibilities.size} percentile index: $tenthPercentileIndex, distances $sortedDistances")
    sortedDistances(tenthPercentileIndex)
  }
  */

  def setDistance(v: VisibilitySet): Double = setDistanceAVG(v)

  /*
  def setDistanceWorst(v: VisibilitySet): Double = {
    v.visibilities.map(visibility => distance(visibility)).max
  }*/

  def setDistanceAVG(v: VisibilitySet): Double = {
    v.visibilities.map(visibility => distance(visibility)).sum / visibilities.size
  }


  def center(verbose: Boolean = false): Visibility = {
    if (verbose) println(s"Distances for determining center for $color ${visibilities.map(v => (v.p, visibilities.map(_.distance(v)).sum)).sortBy(_._2)}")
    visibilities.map(v => (v, visibilities.map(_.distance(v)).sum)).minBy(_._2)._1
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

  def clusterInitial(visibility: Set[Visibility], e: Environment, size: Int, verbose: Boolean = false): EnvironmentSegmentation = {
    val colors = takeColors(size)
    var (remaining, clusters) = initialStarters(visibility, size)
    clusterPostInitialization(remaining, clusters, e, verbose)
  }

  def clusterPostInitialization(remaining: Set[Visibility], clusters: MutableSeq[VisibilitySet], e: Environment, verbose: Boolean) = {
    remaining.foreach { v =>
      val minGroup = clusters.minBy(_.distance(v))
      val index = clusters.indexOf(minGroup)
      if (verbose) {
        println(s"adding ${v.p} to ${clusters(index).color} distances were ${clusters.map(c => (c.color, c.distance(v)))}")
      }
      clusters(index) = VisibilitySet(minGroup.visibilities :+ v, minGroup.color)
    }
    EnvironmentSegmentation(clusters, e)
  }

  def initialStarters(visibility: Set[Visibility], size: Int, verbose: Boolean = false): (Set[Visibility], MutableSeq[VisibilitySet]) = {
    var vis = visibility
    var clusters: MutableSeq[VisibilitySet] = MutableSeq.empty
    (0 until size).foreach { i =>
      val (taken: Visibility, left: Set[Visibility]) = PointClustering.takeRandom(visibility)
      vis = left
      clusters = clusters :+ VisibilitySet(Seq(taken), colors(i))
    }
    if (verbose) {
      val out = clusters.map(c => (c.color, c.visibilities.map(_.p)))
      println(s"cluster initialization points ${out}")
    }
    assert(clusters.size == size)
    (vis, clusters)
  }

  def convertCentersToVisibilitySet(centers: Seq[Visibility]): MutableSeq[VisibilitySet] = {
    var res: MutableSeq[VisibilitySet] = MutableSeq.empty
    val colors = takeColors(centers.size)
    centers.zipWithIndex.foreach { case (v: Visibility, i: Int) =>
      res = res :+ VisibilitySet(Seq(v), colors(i))
    }
    res
  }

  def clusterRoundsSeq(points: Set[Point], e: Environment, size: Int, rounds: Int, verbose: Boolean = false): Seq[EnvironmentSegmentation] = {
    var segmentations: MutableSeq[EnvironmentSegmentation] = MutableSeq.empty
    var visibility: Set[Visibility] = determineVisibility(points, e).visibility
    var segmentation = clusterInitial(visibility, e, size, verbose)
    segmentations = segmentations :+ segmentation
    var centers = convertCentersToVisibilitySet(segmentation.partition.map(_.center(verbose)))
    if (verbose) println(s"Center Initial: ${segmentation.partition.map(v => (v.center(verbose), v.color)).sortBy { case (center, color) => (center.p.x, center.p.y) }.map(_._1.p)}\n")
    var others = segmentation.partition.flatMap(_.visibilities);
    var round = 1
    while(round < rounds) {
      segmentation = clusterPostInitialization(visibility.filterNot(centers.contains(_)), centers, e, verbose)
      segmentations = segmentations :+ segmentation
      centers = convertCentersToVisibilitySet(segmentation.partition.map(_.center(verbose)))
      if (verbose) {
        println(s"ROUND: $round")
        println(s"Center Initial: ${segmentation.partition.map(v => (v.center(verbose), v.color)).sortBy { case (center, color) => (center.p.x, center.p.y) }.map(_._1.p)}\n")
      }
      others = segmentation.partition.flatMap(_.visibilities);
      round += 1
    }
    segmentations
  }

  def clusterRounds(points: Set[Point], e: Environment, size: Int, rounds: Int, verbose: Boolean = false): EnvironmentSegmentation = {
    clusterRoundsSeq(points, e, size, rounds, verbose).last
  }
}


