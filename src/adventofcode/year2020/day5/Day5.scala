package adventofcode.year2020.day5

import scala.io.Source

object Day5 {

  def main(args: Array[String]): Unit = {
    val tickets = Source.fromFile("src/adventofcode/year2020/day5/input.txt").getLines()
    val seatIds = tickets.map(_.replace('F','0'))
      .map(_.replace('B','1'))
      .map(_.replace('R','1'))
      .map(_.replace('L','0'))
      .map(Integer.parseInt(_, 2)).toSeq

    println(seatIds.max)
    val suitable = seatIds
      .filter(id => (id & 0xFF8) != 0xFF8)// filter the back
      .filter(id => (id & 0xFF8) != 0)// filter the front
      .sorted
    val availableSeat = suitable
      .zip(suitable.tail)   //pair up adjacent seatc
      .filter(pair => pair._2-pair._1==2)
      .map(pair => pair._1+1)
      .head
    print(availableSeat)
  }
}
