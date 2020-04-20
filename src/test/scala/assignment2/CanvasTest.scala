package assignment2

import org.specs2.mutable.Specification

class CanvasTest extends Specification {

  "Canvas" should {

    "create canvas if provided coordinates are valid" in {
      val expected = new Canvas(50, 50)
      val res = Canvas.apply(50, 50)
      res must beRight(expected)
    }

    "not create canvas if provided upper Xcoordinates are not valid" in {
      val expected: Error = InvalidCanvasXCordinate
      val res = Canvas.apply(5000, 1000)
      res must beLeft(expected)
    }

    "not create canvas if provided upper Ycoordinates are not valid" in {
      val expected: Error = InvalidCanvasYCordinate
      val res = Canvas.apply(1000, 5000)
      res must beLeft(expected)
    }

  }

}
