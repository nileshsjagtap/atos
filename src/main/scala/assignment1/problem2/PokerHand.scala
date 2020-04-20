package assignment1.problem2

sealed abstract class PokerHand {
  def fulfillsCriteria: List[Card] => Boolean

  val priority: Int

  def winningCards : List[Card]

  protected def checkForSequence(list: List[Int]): Boolean = list match {
    case Nil => true
    case _ :: Nil => true
    case f :: s :: _ if s - f != 1 => false
    case f :: s :: tail if s - f == 1 => checkForSequence(s :: tail)
  }
}

case class RoyalFlush(cards: List[Card]) extends PokerHand {
  override def fulfillsCriteria: List[Card] => Boolean = cards => {
    val list = cards.map(_.rank.abbreviation).map(Rank.pointValueOf).sortWith(_ < _)
    checkForSequence(list) && cards.groupBy(_.suit).size == 1 && isRoyal(list)
  }

  override def winningCards: List[Card] = ???

  private def isRoyal(list: List[Int]) = List(10, 11, 12, 13, 14).count(list.contains(_)) == 5

  override val priority = 1
}

case class StraightFlush(cards: List[Card]) extends PokerHand {
  override def fulfillsCriteria: List[Card] => Boolean = cards => {
    val list = cards.map(_.rank.abbreviation).map(Rank.pointValueOf).sortWith(_ < _)
    checkForSequence(list) && cards.groupBy(_.suit).size == 1
  }

  override def winningCards: List[Card] = ???

  override val priority = 2
}

case class FourOfAKind(cards: List[Card]) extends PokerHand {
  override def fulfillsCriteria: List[Card] => Boolean =
    _.groupBy(_.rank).values.toList.count(_.size == 4) == 1

  override def winningCards: List[Card] = cards.groupBy(_.rank).values.toList.find(_.size == 4).get

  override val priority = 3
}

case class FullHouse(cards: List[Card]) extends PokerHand {
  override def fulfillsCriteria: List[Card] => Boolean = cards => {
    val list = cards.groupBy(_.rank).values.toList
    list.size == 2 && list.count(_.size == 2) == 1 && list.count(_.size == 3) == 1
  }

  override def winningCards: List[Card] = cards.groupBy(_.rank).values.toList.find(_.size == 3).get

  override val priority = 4
}

case class Flush(cards: List[Card]) extends PokerHand {
  override def fulfillsCriteria: List[Card] => Boolean =
    _.groupBy(_.suit).size == 1

  override def winningCards: List[Card] = ???

  override val priority = 5
}

case class Straight(cards: List[Card]) extends PokerHand {
  override def fulfillsCriteria: List[Card] => Boolean = cards => {
    val list = cards.map(_.rank.abbreviation).map(Rank.pointValueOf).sortWith(_ < _)
    checkForSequence(list)
  }

  override def winningCards: List[Card] = ???

  override val priority = 6
}

case class ThreeOfAKind(cards: List[Card]) extends PokerHand {
  override def fulfillsCriteria: List[Card] => Boolean =
    _.groupBy(_.rank).values.toList.count(list => list.size == 3) == 1

  override def winningCards: List[Card] = cards.groupBy(_.rank).values.toList.find(list => list.size == 3).get

  override val priority = 7
}

case class TwoPair(cards: List[Card]) extends PokerHand {
  override def fulfillsCriteria: List[Card] => Boolean =
    _.groupBy(_.rank).values.toList.count(list => list.size == 2) == 2

  override def winningCards: List[Card] = cards.groupBy(_.rank).values.toList.find(list => list.size == 2).get

  override val priority = 8
}

case class OnePair(cards: List[Card]) extends PokerHand {
  override def fulfillsCriteria: List[Card] => Boolean =
    _.groupBy(_.rank).values.toList.count(list => list.size == 2) == 1

  override def winningCards = cards.groupBy(_.rank).values.toList.find(list => list.size == 2).get

  override val priority = 9
}

case class HighCard(cards: List[Card]) extends PokerHand {
  override def fulfillsCriteria: List[Card] => Boolean = cards =>
    cards.groupBy(_.rank).size == 5 && cards.groupBy(_.suit).size != 1

  override def winningCards: List[Card] = List(cards.sortBy(c => Rank.pointValueOf(c.rank.abbreviation)).last)

  override val priority = 10
}

object PokerHand {

  private val pokerHandsList = List(
    RoyalFlush,
    StraightFlush,
    FourOfAKind,
    FullHouse,
    Flush,
    Straight,
    ThreeOfAKind,
    TwoPair,
    OnePair,
    HighCard
  )

  def apply(cards: List[Card]) =
    if (cards.size > 5) Left(InvalidNumberOfCardsDistributedError)
    else Right(findPokerHand(cards))

  private def findPokerHand(cards: List[Card]): PokerHand =
    pokerHandsList.find(hand => hand(cards).fulfillsCriteria(cards)).getOrElse(HighCard)(cards)

}
