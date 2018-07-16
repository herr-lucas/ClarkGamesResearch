abstract class Geo(val id: Option[Int], val name: Option[String], val verbose: Boolean, val specialColor: Option[String] = None) {
  def isVisible(g: Geo, e: Environment): Boolean
  def intersect(pointAndTwoDVector: PointAndTwoDVector): Boolean
  def intersectDistance(pointAndTwoDVector: PointAndTwoDVector): Option[Double]
  def intersectPoint(pointAndTwoDVector: PointAndTwoDVector): Option[Point]
}