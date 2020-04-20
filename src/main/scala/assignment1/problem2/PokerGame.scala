package assignment1.problem2

object PokerGame {

  def winner(players: List[Player]) =
    if (players.count(_.cards.isEmpty) > 0) Left(HandsNotAssignedError)
    else pokerHandsFor(players).map(_.winner)

  def pokerHandsFor(players: List[Player]): Either[Error, PlayerPokerHands] =
    if (players.count(_.cards.isEmpty) > 0) Left(HandsNotAssignedError)
    else sequence(players.map(p => PokerHand(p.cards))).map(phs => players.zip(phs)).map(tup => tup.map(PlayerPokerHand.tupled)).map(PlayerPokerHands)

  private def sequence[L, R](listOfEither: List[Either[L, R]]): Either[L, List[R]] = {
    def iterate(remaining: List[Either[L, R]], buffer: Either[L, List[R]]): Either[L, List[R]] = remaining match {
      case Nil => buffer
      case head :: _ if head.isLeft => Left(head.left.get)
      case head :: tail => iterate(tail, Right(buffer.right.get :+ head.right.get))
    }
    iterate(listOfEither, Right(List[R]()))
  }

}