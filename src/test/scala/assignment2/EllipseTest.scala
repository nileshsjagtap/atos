package assignment2

import org.specs2.mutable.Specification

class EllipseTest extends Specification {

  "Ellipse" should {

    "create Ellipse if provided coordinates are valid" in {
      val expected = new Ellipse(50, 50, 10, 20)
      val res = Ellipse.apply(50, 50, 10, 20, new Canvas(1000, 1000))
      res must beRight(expected)
    }

    "not create Ellipse if provided upper Xcoordinates are not valid" in {
      val expected: Error = InvalidXCordinate
      val res = Ellipse.apply(5000, 1000, 10, 20, new Canvas(1000, 1000))
      res must beLeft(expected)
    }

    "not create Ellipse if provided upper Ycoordinates are not valid" in {
      val expected: Error = InvalidYCordinate
      val res = Ellipse.apply(1000, 5000, 10, 20, new Canvas(1000, 1000))
      res must beLeft(expected)
    }

    "not create Ellipse if provided horizontal diameter is not valid" in {
      val expected: Error = invalidHd
      val res = Ellipse.apply(1000, 1000, -10, 20, new Canvas(1000, 1000))
      res must beLeft(expected)
    }

    "not create Ellipse if provided verticle diameter is not valid" in {
      val expected: Error = invalidVd
      val res = Ellipse.apply(1000, 1000, 10, -20, new Canvas(1000, 1000))
      res must beLeft(expected)
    }

  }


}
