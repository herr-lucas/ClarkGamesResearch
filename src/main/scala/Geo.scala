abstract class Geo(val id: Option[Int], val name: Option[String], val verbose: Boolean) {
  def isVisible(g: Geo, e: Environment): Boolean
  def intersect(pointAndTwoDVector: PointAndTwoDVector): Boolean
  def intersectDistance(pointAndTwoDVector: PointAndTwoDVector): Option[Double]
}