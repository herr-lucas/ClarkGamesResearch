object Main {
  import BasicTests._
  import EnvironmentExtractor._
  def main(args: Array[String]): Unit = {
    simple3PointIntersectionTest
    simple3PointNoIntersectionTest
    simpleTwoLineSegmentIntersection
    simpleXMLLoadTest
    simpleLineSegmentContainsTest
    simpleThreeLineSegmentVisibilityTest
    simpleThreeLineSegmentNoVisibilityTest
    complicatedThreeLineSegmentVisiblityTest
    complicatedThreeLineSegmentNoVisibilityTest
  }
}
