package assignment2

trait MaterialBillGenerator[T] {

  def generateBill(input: List[Widget]): Either[Error, String]

}

object MaterialBillGenerator {

  implicit object billForBuilder extends MaterialBillGenerator[BuilderSystem.type] {
    override def generateBill(widgets: List[Widget]) = {
      val op = widgets.map(widget => widget match {
        case Rectangle(px, py, w, h) => Right(s"Rectangle(${px},${py}) width=${w} height=${h}")
        case Square(px, py, w) => Right(s"Square(${px},${py}) size=${w}")
        case Ellipse(px, py, hd, vd) => Right(s"Ellipse(${px},${py}) diameterH=${hd} diameterV=${vd}")
        case Circle(px, py, d) => Right(s"Circle(${px},${py}) size=${d}")
        case TextBox(px, py, w, h, t) => Right(s"TextBox(${px},${py}) width=${w} height=${h} text=${t}")
        case _ => Left(InvalidWidget)
      })
      OutPutForBuilder(MaterialBill.sequence(op)).outputString
    }
  }

}

case class OutPutForBuilder(op: Either[Error, List[String]]) {

  val commonString = "---------------------------------" + "\n" + "Bill Of Materials" + "\n" + "--------------------------------- "

  def outputString = op match {
    case Right(value) => Right(value.foldLeft(commonString)((acc, y) => acc + "\n" + y))
    case Left(err) => Left(err)
  }
}

object MaterialBill {

  def apply[W](in: List[Widget])(implicit mb: MaterialBillGenerator[W]) = mb.generateBill(in)

  def sequence[L, R](listOfEither: List[Either[L, R]]): Either[L, List[R]] = {
    def iterate(remaining: List[Either[L, R]], buffer: Either[L, List[R]]): Either[L, List[R]] = remaining match {
      case Nil => buffer
      case head :: _ if head.isLeft => Left(head.left.get)
      case head :: tail => iterate(tail, Right(buffer.right.get :+ head.right.get))
    }

    iterate(listOfEither, Right(List[R]()))
  }

}