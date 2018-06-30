case class EnvironmentPartition(points: Seq[Point], border: Border) {

  def split(lineSegment: LineSegment): (EnvironmentPartition, EnvironmentPartition) = {
    (this, this)
  }
}
