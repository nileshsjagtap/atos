package assignment2

case class TextBox(positionX: Int, positionY: Int, width:Int, height:Int, text: String) extends Widget

object TextBox {

  def apply(positionX: Int, positionY: Int, width:Int, height:Int,  canvas: Canvas, text: String = ""): Either[Error, TextBox] = {
    for{
      validXCordinate <- validateXCordinate(positionX, canvas)
      validYCordinate <- validateYCordinate(positionY, canvas)
      validWidth <- validateWidth(width)
      validHeight <- validateHeight(height)
    } yield new TextBox(validXCordinate, validYCordinate, validWidth, validHeight, text)
  }

  private def validateXCordinate= (xCordinate: Int, canvas: Canvas) => Either.cond(xCordinate < canvas.upperXCordinate, xCordinate, InvalidXCordinate)

  private def validateYCordinate= (yCordinate: Int, canvas: Canvas) => Either.cond(yCordinate < canvas.upperYCordinate, yCordinate, InvalidYCordinate)

  private def validateWidth= (width: Int) => Either.cond(width > 0, width, invalidWidth)

  private def validateHeight= (height: Int) => Either.cond(height > 0, height, invalidHeight)
}

