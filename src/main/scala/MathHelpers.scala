object MathHelpers {
  val NumericalIntersectionError = 0.0001
  def relativeError(x: Double, y: Double): Double = {
    if (x + y == 0) 0
    else Math.abs((x - y) * 2 / (x + y))
  }
  def error(x: Double, y: Double): Double = {
    Math.abs(x - y)
  }
  def sameSign(x: Double, y: Double): Boolean = {
    (x * y >= 0)
  }
}