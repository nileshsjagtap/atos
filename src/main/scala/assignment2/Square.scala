package assignment2

case class Square(positionX: Int, positionY: Int, width:Int) extends Widget

object Square {

  def apply(positionX: Int, positionY: Int, width: Int, canvas: Canvas): Either[Error, Square] = {
    for{
      validXCordinate <- validateXCordinate(positionX, canvas)
      validYCordinate <- validateYCordinate(positionY, canvas)
      validWidth <- validateWidth(width)
    } yield Square(validXCordinate, validYCordinate, validWidth)
  }

  private def validateXCordinate= (xCordinate: Int, canvas: Canvas) => Either.cond(xCordinate < canvas.upperXCordinate, xCordinate, InvalidXCordinate)

  private def validateYCordinate= (yCordinate: Int, canvas: Canvas) => Either.cond(yCordinate < canvas.upperYCordinate, yCordinate, InvalidYCordinate)

  private def validateWidth= (width: Int) => Either.cond(width > 0, width, invalidWidth)

}
