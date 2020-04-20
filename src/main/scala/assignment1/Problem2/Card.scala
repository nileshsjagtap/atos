package assignment1.Problem2

case class Card (suit: Suit, rank: Rank)

object Card {

  def apply(rank: String, suit: String): Either[Error, Card] = for {
    suit <- Suit(suit)
    rank <- Rank(rank)
  } yield Card(suit, rank)

  def createCards(cards: List[String]) = {
    sequence(cards.map{card => Card(card.charAt(0).toString, card.charAt(1).toString)})
  }

  private def sequence[L, R](listOfEither: List[Either[L, R]]): Either[L, List[R]] = {
    def iterate(remaining: List[Either[L, R]], buffer: Either[L, List[R]]): Either[L, List[R]] = remaining match {
      case Nil => buffer
      case head :: _ if head.isLeft => Left(head.left.get)
      case head :: tail => iterate(tail, Right(buffer.right.get :+ head.right.get))
    }
    iterate(listOfEither, Right(List[R]()))
  }

}