package assignment1.Problem2

case class Player(val name: String, val cards: List[Card])

object Player {

  def apply(name: String): Either[InvalidPlayerError.type, Player] =
    if (name.nonEmpty) Right(new Player(name, List()))
    else Left(InvalidPlayerError)

  def apply(name: String, cards: List[Card]): Either[InvalidPlayerError.type, Player] =
    if (name.nonEmpty && cards.nonEmpty) Right(new Player(name, cards))
    else Left(InvalidPlayerError)

  def assignCards(player: Player, cards: List[Card]) = new Player(player.name, cards)

}

case class PlayerPokerHand(player: Player, pokerHand: PokerHand)

case class PlayerPokerHands(list: List[PlayerPokerHand]) {

  def winner = {
    val sorted = list.zip(list.map(_.pokerHand.priority)).sortWith(_._2 < _._2)
    if (isTie(sorted)) {
      clearWinner(sorted.map(_._1))
    }
    else
      sorted.filter(_._2 == sorted.head._2).map(_._1)
  }

  def clearWinner(sorted: List[PlayerPokerHand]): List[PlayerPokerHand] = {
    (sorted.head.pokerHand, sorted.last.pokerHand) match {
      case (OnePair(_), OnePair(_)) if (sorted.head.pokerHand.winningCards.map(_.rank).head == sorted.last.pokerHand.winningCards.map(_.rank).head) => {
        val p1 = createUpdatedPlayers(sorted.head.player, sorted.head.pokerHand.winningCards)
        val p2 = createUpdatedPlayers(sorted.last.player, sorted.last.pokerHand.winningCards)
        val sortedPlayer = sort(PokerGame.pokerHandsFor(List(p1, p2)).right.get.list)
        clearWinner(sortedPlayer.map(_._1))
      }
      case (OnePair(_), OnePair(_)) => List(max(sorted.head, sorted.last))
      case (HighCard(_), HighCard(_)) => List(max(sorted.head, sorted.last))
    }
  }

  def sort(list: List[PlayerPokerHand]) = list.zip(list.map(_.pokerHand.priority)).sortWith(_._2 < _._2)

  def createUpdatedPlayers(p1: Player, cardsToRemove: List[Card]) = p1.copy(cards = p1.cards diff cardsToRemove)

  def max(p1: PlayerPokerHand, p2: PlayerPokerHand): PlayerPokerHand = if (p1.pokerHand.winningCards.map(_.rank.abbreviation).map(Rank.pointValueOf).head > p2.pokerHand.winningCards.map(_.rank.abbreviation).map(Rank.pointValueOf).head) p1 else p2

  def isTie(players: List[(PlayerPokerHand, Int)]) = players.forall(_._2 == players.head._2)
}
