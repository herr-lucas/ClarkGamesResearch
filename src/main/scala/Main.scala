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
  }
}
