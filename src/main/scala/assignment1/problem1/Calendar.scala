package assignment1.problem1

import java.util.Calendar
import assignment1.FileWriter
import scala.annotation.tailrec

class CalendarProblem {

  def countOfSundaysOnFirstOfMonth(fromYear: Int, toYear: Int) = {

    val calendar = Calendar.getInstance()

    @tailrec
    def iterateYears(year: Int, count: Int): Int = year match {
      case d if toYear + 1 == d => count
      case _ => iterateYears(year + 1, iterateMonths(year, 1, 1, count))
    }

    @tailrec
    def iterateMonths(year: Int, month: Int, day: Int, count: Int): Int = month match {
      case 13 => count
      case _ => iterateMonths(year, month + 1, day, if (isSunday(year, month, day, count)) count + 1 else count)
    }

    def isSunday(year: Int, month: Int, day: Int, count: Int) = {
      calendar.set(year, month, 1)
      calendar.get(Calendar.DAY_OF_WEEK).equals(Calendar.SUNDAY)
    }

    iterateYears(fromYear, 0)
  }
}

object Run extends App {
  val countOfSundays = new CalendarProblem().countOfSundaysOnFirstOfMonth(1901, 2000)
  FileWriter.write(s"count of sundays on first of month from 1901 to 200 is ${countOfSundays}", "src/main/scala/assignment1/Problem1/output1")
}
