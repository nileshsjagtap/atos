package assignment1.Problem2

sealed abstract class Suit {
  def get = this.toString
  def abbreviation = this.toString.head.toString
}

case object H extends Suit
case object S extends Suit
case object D extends Suit
case object C extends Suit

object Suit {

  def allSuits = suitMap.values.toList

  private def suitMap = Map(
    H.get.toLowerCase -> H,
    S.get.toLowerCase -> S,
    D.get.toLowerCase -> D,
    C.get.toLowerCase -> C
  )

  def apply(suit: String): Either[Error, Suit] = {
    if (validSuit(suit)) Right(suitMap(suit.toLowerCase))
    else Left(InvalidSuitError)
  }

  private def validSuit(suit: String) = suit.nonEmpty && suitMap.keySet.map(_.toLowerCase).contains(suit.toLowerCase)

}