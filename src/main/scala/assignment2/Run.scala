package assignment2

import assignment1.FileWriter

object Run extends App {

  for {
    canvas <- Canvas(1000, 1000)
    rectangle <- Rectangle(10, 10, 30, 40, canvas)
    square <- Square(15, 30, 35, canvas)
    ellipse <- Ellipse(100, 150, 300, 200, canvas)
    circle <- Circle(1, 1, 300, canvas)
    textBox <- TextBox(5, 5, 200, 100, canvas, "sample text")
    bill <- MaterialBill[BuilderSystem.type](List(rectangle, square, ellipse, circle, textBox))
  } yield {
    FileWriter.write(bill,"src/main/scala/assignment2/output")
  }

}
