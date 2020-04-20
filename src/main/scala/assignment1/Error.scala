package assignment1

trait Error

case object FileNotFound extends Error

case class ErrorReadingFile(err: String) extends Error

case class ErrorWritingFile(err: String) extends Error
