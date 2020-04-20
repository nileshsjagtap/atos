package assignment2

case class Circle(positionX: Int, positionY: Int, diameter:Int) extends Widget

object Circle {

  def apply(positionX: Int, positionY: Int, diameter:Int, canvas: Canvas): Either[Error, Circle] = {
    for{
      validXCordinate <- validateXCordinate(positionX, canvas)
      validYCordinate <- validateYCordinate(positionY, canvas)
      validDiameter <- validateDiameter(diameter)
    } yield Circle(validXCordinate, validYCordinate, validDiameter)
  }

  private def validateXCordinate= (xCordinate: Int, canvas: Canvas) => Either.cond(xCordinate < canvas.upperXCordinate, xCordinate, InvalidXCordinate)

  private def validateYCordinate= (yCordinate: Int, canvas: Canvas) => Either.cond(yCordinate < canvas.upperYCordinate, yCordinate, InvalidYCordinate)

  private def validateDiameter= (diameter: Int) => Either.cond(diameter > 0, diameter, invalidDiameter)

}
