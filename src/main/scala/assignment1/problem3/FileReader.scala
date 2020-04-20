package assignment1.problem3

import scala.io.Source
import scala.util.Try

trait Reader[A, B] {
  def read(input: A): B
}


object FileReader extends Reader[String, Try[Array[String]]]{

  override def read(filePath: String): Try[Array[String]] = {
      Try(Source.fromFile(filePath).getLines().toArray)
  }

}
