package assignment2

import org.specs2.mutable.Specification

class RectangleTest extends Specification {

  "Rectangle" should {

    "create Rectangle if provided coordinates are valid" in {
      val expected = new Rectangle(50, 50, 10, 20)
      val res = Rectangle.apply(50, 50, 10, 20, new Canvas(1000, 1000))
      res must beRight(expected)
    }

    "not create Rectangle if provided upper Xcoordinates are not valid" in {
      val expected: Error = InvalidXCordinate
      val res = Rectangle.apply(5000, 1000, 10, 20, new Canvas(1000, 1000))
      res must beLeft(expected)
    }

    "not create Rectangle if provided upper Ycoordinates are not valid" in {
      val expected: Error = InvalidYCordinate
      val res = Rectangle.apply(1000, 5000, 10, 20, new Canvas(1000, 1000))
      res must beLeft(expected)
    }

    "not create Rectangle if provided widthr is not valid" in {
      val expected: Error = invalidWidth
      val res = Rectangle.apply(1000, 1000, -10, 20, new Canvas(1000, 1000))
      res must beLeft(expected)
    }

    "not create Rectangle if provided height is not valid" in {
      val expected: Error = invalidHeight
      val res = Rectangle.apply(1000, 1000, 10, -20, new Canvas(1000, 1000))
      res must beLeft(expected)
    }

  }

}
