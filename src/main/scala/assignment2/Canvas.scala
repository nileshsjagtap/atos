package assignment2

case class Canvas(upperXCordinate: Int, upperYCordinate: Int)

object Canvas {

  def apply(upperXCordinate: Int, upperYCordinate: Int): Either[Error, Canvas] = {
    for {
      validUpperXCordinate <- validateUpperXCordinate(upperXCordinate)
      validUpperYCordinate <- validateUpperYCordinate(upperYCordinate)
    } yield new Canvas(validUpperXCordinate, validUpperYCordinate)
  }

  private def validateUpperXCordinate = (upperXCordinate: Int) => Either.cond(upperXCordinate > 0, upperXCordinate, InvalidCanvasXCordinate)

  private def validateUpperYCordinate = (upperYCordinate: Int) => Either.cond(upperYCordinate > 0, upperYCordinate, InvalidCanvasYCordinate)

}
