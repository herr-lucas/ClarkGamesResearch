object MathHelpers {
  val NumericalIntersectionError = 0.0001
  def closeEnough(v1: Double, v2: Double): Boolean = {
    // relative error has issues with both x's are 0 in x1 * y2 = x2 * y1
    error(v1, v2) < NumericalIntersectionError
  }

  def error(x: Double, y: Double): Double = {
    Math.abs(x - y)
  }
  def sameSign(x: Double, y: Double): Boolean = {
    (x * y >= 0)
  }
}