package assignment1.Problem2

import assignment1.FileWriter

object Run extends App {

  val fileInput = FileReader.read("target/resources/poker").fold(err => println(err), handsList => findWinner(handsList))

  def findWinner(handsList: List[String]) = {
    val list = handsList.map { hands =>
      for {
        handForFirstPlayer <- Card.createCards(hands.split(" ").toList.take(5))
        handForSecondPlayer <- Card.createCards(hands.split(" ").toList.drop(5))
        player1 <- Player("player1", handForFirstPlayer)
        player2 <- Player("player2", handForSecondPlayer)
        winningPlayer <- PokerGame.winner(List(player1, player2))
      } yield {
        winningPlayer
      }
    }
    sequence(list) match {
      case Right(listOfWinningPlayer) => {
        val count = listOfWinningPlayer.flatten.filter(p => p.player.name == "player1").size
        FileWriter.write(s"Player1 won ${count} hands", "src/main/scala/assignment1/Problem2/output2")
      }
      case Left(err) => FileWriter.write(s"err ${err}", "src/main/scala/assignment1/Problem2/output2")
    }

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
