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
    loadAndRenderSimplePath
    // simple visibility test
    val fromPoints = Seq(Point(0,0), Point(10, 10), Point(10, 40), Point(40, 10), Point(76, 76), Point(90, 76))
    fromPoints.foreach { p => testVisibilitySimpleBoxEnv(p) }
    // loadAndRender CallofDutyMap
    loadCallofDutyMap
    renderCallOfDutyMap
    // test points call of Duty map
    val fromPointsCOD = Seq(Point(100, 100), Point(700, 100), Point(100, 700), Point(500, 500))
    fromPointsCOD.foreach { p => testCallOfDutyEnvironment(p) }
    // debugging weird behavior
    // temporaryCODDebuggingTests
    // environment segmentation
    distanceTest()
    basicVisibilityTest()
    basicSegmentationTest()
    environmentSegmentationTest()
    environmentSegmentationCODTest()
  }
}
