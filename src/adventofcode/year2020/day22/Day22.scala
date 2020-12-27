package adventofcode.year2020.day22

import scala.io.Source

object Day22 {

  type FnPlay = (Seq[List[Int]], Seq[List[Int]]) => (Seq[List[Int]], Boolean, Seq[List[Int]], Boolean)
  def main(args: Array[String]): Unit = {
    //run("src/adventofcode/year2020/day22/inputExample.txt", playRoundpart1 _)
    //run("src/adventofcode/year2020/day22/input.txt", playRoundpart1 _)
    run("src/adventofcode/year2020/day22/inputExample.txt", playRoundpart2 _)
    run("src/adventofcode/year2020/day22/input.txt", playRoundpart2 _)
  }
  def run(inputFile: String, fnPlay:FnPlay) = {
    val lines = Source.fromFile(inputFile).getLines()
    val player1 = lines.takeWhile(!_.isEmpty).toList.tail.map(_.toInt)
    val player2 = lines.toList.tail.map(_.toInt)

    val rounds = playGame(fnPlay, player1, player2)
    val zippedRounds = rounds._2.zip(rounds._4).zipWithIndex
    println(zippedRounds.map(zr => {
      s"${zr._2+1}) Player1: ${zr._1._1.mkString(",")}; Player2: ${zr._1._2.mkString(",")}"
    }).mkString("\n"))
    val player1Rounds = rounds._2.reverse.head
    val player2Rounds = rounds._4.reverse.head
    val score = player1Rounds.zipWithIndex.map(card => card._1*(player1Rounds.length-card._2)).sum + player2Rounds.zipWithIndex.map(card => card._1*(player2Rounds.length-card._2)).sum
    println(score)
  }

  private def playGame(fnPlay: FnPlay, player1: List[Int], player2: List[Int]) = {
    Stream.from(1).scanLeft((false, Seq(player1), false, Seq(player2), false))((acc, _) => {
      val (_, player1Rounds, player1Win, player2Rounds, player2Win) = acc
      if (player1Win || player2Win) {
        (true, player1Rounds, player1Win, player2Rounds, player2Win)
      } else {
        val r = fnPlay(player1Rounds, player2Rounds)
        (false, r._1, r._2, r._3, r._4)
      }
    }).takeWhile(!_._1).reverse.head
  }

  private def playRoundpart1(player1Rounds: Seq[List[Int]], player2Rounds: Seq[List[Int]]) : (Seq[List[Int]], Boolean, Seq[List[Int]], Boolean) = {
    val player1 = player1Rounds.reverse.head
    val player2 = player2Rounds.reverse.head
    val head1: Int = player1.head
    val head2: Int = player2.head
    if (head1 > head2) {
      val newPlayer1Round = player1.tail ++ List(head1, head2)
      val newPlayer2Round = player2.tail
      (player1Rounds :+ newPlayer1Round, player2.tail.isEmpty, player2Rounds :+ newPlayer2Round, false)
    } else {
      val newPlayer1Round = player1.tail
      val newPlayer2Round = player2.tail ++ List(head2, head1)
      (player1Rounds :+ newPlayer1Round, false, player2Rounds :+ newPlayer2Round, player1.tail.isEmpty)

    }
  }

  private def playRoundpart2(player1Rounds: Seq[List[Int]], player2Rounds: Seq[List[Int]]) : (Seq[List[Int]], Boolean, Seq[List[Int]], Boolean) = {
    val player1 = player1Rounds.reverse.head
    val player2 = player2Rounds.reverse.head
    val head1: Int = player1.head
    val head2: Int = player2.head
    if (player1Rounds.filter(_==player1).size>1 || player2Rounds.filter(_==player2).size>1) {
      (player1Rounds , true, player2Rounds , false)
    } else if(head1 < player1.size && head2 < player2.size) {
      val subGame = playGame(playRoundpart2 _, player1.tail.slice(0, head1), player2.tail.slice(0, head2))
      if(subGame._3) {
        val newPlayer1Round = player1.tail ++ List(head1, head2)
        val newPlayer2Round = player2.tail
        (player1Rounds :+ newPlayer1Round, player2.tail.isEmpty, player2Rounds :+ newPlayer2Round, false)
      } else {
        val newPlayer1Round = player1.tail
        val newPlayer2Round = player2.tail ++ List(head2, head1)
        (player1Rounds :+ newPlayer1Round, false, player2Rounds :+ newPlayer2Round, player1.tail.isEmpty)

      }
    } else if (head1 > head2) {
      val newPlayer1Round = player1.tail ++ List(head1, head2)
      val newPlayer2Round = player2.tail
      (player1Rounds :+ newPlayer1Round, player2.tail.isEmpty, player2Rounds :+ newPlayer2Round, false)
    } else {
      val newPlayer1Round = player1.tail
      val newPlayer2Round = player2.tail ++ List(head2, head1)
      (player1Rounds :+ newPlayer1Round, false, player2Rounds :+ newPlayer2Round, player1.tail.isEmpty)

    }
  }
}
