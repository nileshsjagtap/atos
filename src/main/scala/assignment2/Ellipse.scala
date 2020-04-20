package assignment2

case class Ellipse(positionX: Int, positionY: Int, horizontalDiamete:Int, verticalDiameter:Int) extends Widget

object Ellipse {

  def apply(positionX: Int, positionY: Int, horizontalDiamete:Int, verticalDiameter:Int, canvas: Canvas): Either[Error, Ellipse] = {
    for{
      validXCordinate <- validateXCordinate(positionX, canvas)
      validYCordinate <- validateYCordinate(positionY, canvas)
      validHd <- validateHorizontalDiameter(horizontalDiamete)
      validVd <- validateVerticalDiameter(verticalDiameter)
    } yield Ellipse(validXCordinate, validYCordinate, validHd, validVd)
  }

  private def validateXCordinate= (xCordinate: Int, canvas: Canvas) => Either.cond(xCordinate < canvas.upperXCordinate, xCordinate, InvalidXCordinate)

  private def validateYCordinate= (yCordinate: Int, canvas: Canvas) => Either.cond(yCordinate < canvas.upperYCordinate, yCordinate, InvalidYCordinate)

  private def validateHorizontalDiameter= (validHd: Int) => Either.cond(validHd > 0, validHd, invalidHd)

  private def validateVerticalDiameter= (validVd: Int) => Either.cond(validVd > 0, validVd, invalidVd)


}

