case class EnvironmentPartition(border: Border) {

  def split(lineSegment: LineSegment): (EnvironmentPartition, EnvironmentPartition) = {
    (this, this)
  }
}
