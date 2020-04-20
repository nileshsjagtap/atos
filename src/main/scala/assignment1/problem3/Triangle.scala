package assignment1.problem3

import assignment1.FileWriter

import scala.util.Try

class Triangle {

  val  s = for {
    fileInput <- FileReader.read("target/resources/triangle")
    intList <- convertToInt(fileInput)
  } yield intList

  def convertToInt(strings: Array[String]) = Try(strings.map(_.split(" ").map(_.toInt)))

  s.fold(err => println(err), in => FileWriter.write(s"Max sum is ${findMaxSum(in, in.length)}", "src/main/scala/assignment1/Problem3/output3"))

  def findMaxSum(intLists: Array[Array[Int]], size: Int) = {
    if (size > 1) {
      intLists(1)(1) = intLists(1)(1) + intLists(0)(0)
      intLists(1)(0) = intLists(1)(0) + intLists(0)(0)

      for (row <- 2 until size) {
        intLists(row)(0) = intLists(row)(0) + intLists(row - 1)(0)
        intLists(row)(row) = intLists(row)(row) + intLists(row - 1)(row - 1)

        for (column <- 1 until row) {
          if (intLists(row)(column) + intLists(row - 1)(column - 1) >= intLists(row)(column) + intLists(row - 1)(column))
            intLists(row)(column) = intLists(row)(column) + intLists(row - 1)(column - 1)
          else
            intLists(row)(column) = intLists(row)(column) + intLists(row - 1)(column)
        }
      }
      intLists(size - 1).max
    }
    else{
      intLists(0)(0)
    }
  }
}

object Run extends App {
  new Triangle()
}
