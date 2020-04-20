package assignment2

import org.specs2.mutable.Specification

class SquareTest extends Specification {

  "Square" should {

    "create Square if provided coordinates are valid" in {
      val expected = new Square(50, 50, 10)
      val res = Square.apply(50, 50, 10, new Canvas(1000, 1000))
      res must beRight(expected)
    }

    "not create Square if provided upper Xcoordinates are not valid" in {
      val expected: Error = InvalidXCordinate
      val res = Square.apply(5000, 1000, 10, new Canvas(1000, 1000))
      res must beLeft(expected)
    }

    "not create Square if provided upper Ycoordinates are not valid" in {
      val expected: Error = InvalidYCordinate
      val res = Square.apply(1000, 5000, 10, new Canvas(1000, 1000))
      res must beLeft(expected)
    }

    "not create Square if provided width is not valid" in {
      val expected: Error = invalidWidth
      val res = Square.apply(1000, 1000, -10, new Canvas(1000, 1000))
      res must beLeft(expected)
    }

  }

}
