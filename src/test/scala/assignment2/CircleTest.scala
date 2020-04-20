package assignment2

import org.specs2.mutable.Specification

class CircleTest extends Specification {

  "Circle" should {

    "create circle if provided coordinates are valid" in {
      val expected = new Circle(50, 50, 10)
      val res = Circle.apply(50, 50, 10, new Canvas(1000, 1000))
      res must beRight(expected)
    }

    "not create circle if provided upper Xcoordinates are not valid" in {
      val expected: Error = InvalidXCordinate
      val res = Circle.apply(5000, 1000, 10, new Canvas(1000, 1000))
      res must beLeft(expected)
    }

    "not create circle if provided upper Ycoordinates are not valid" in {
      val expected: Error = InvalidYCordinate
      val res = Circle.apply(1000, 5000, 10, new Canvas(1000, 1000))
      res must beLeft(expected)
    }

    "not create circle if provided diameter is not valid" in {
      val expected: Error = invalidDiameter
      val res = Circle.apply(1000, 1000, -10, new Canvas(1000, 1000))
      res must beLeft(expected)
    }

  }

}
