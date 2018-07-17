object Main {
  import BasicTests._
  import EnvironmentExtractor._
  def main(args: Array[String]): Unit = {
    // Point tests
    simple3PointIntersectionTest
    simple3PointNoIntersectionTest
    // Line tests
    simpleTwoLineSegmentIntersection
    simpleLineSegmentContainsTest
    simpleThreeLineSegmentVisibilityTest
    simpleThreeLineSegmentNoVisibilityTest
    // Complex line tests
    complicatedThreeLineSegmentVisiblityTest
    complicatedThreeLineSegmentNoVisibilityTest
    // Load tests
    simpleXMLLoadTest
    // Other tests
    simpleInternalPointTest
    // Simple box env
    testSimpleBoxEnv
    // Env partition
    partitionEnvironment
    // rendering
    renderEnvironment
    // load paths
    loadPaths
    // simple visibility plot
    val fromPoints = Seq(Point(0,0), Point(10, 10), Point(10, 40), Point(40, 10), Point(76, 76), Point(90, 76))
    fromPoints.foreach { p => testVisibilitySimpleBoxEnv(p) }
  }
}
