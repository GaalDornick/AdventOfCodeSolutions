package adventofcode.year2019.day3

import scala.io.Source

object Day3 {

  def main(args: Array[String]): Unit = {
    val result = Source.fromFile("src/adventofcode/year2019/day3/input.txt").getLines()
      .map(_.split(","))
      .map(parseInstructions(_))
      .map(calculatePositions(_))
      .reduce(_.intersect(_))
      .map(calculatManhattanDistance(_))
      .filter(_ > 0)
      .sorted
    println(result.mkString(","))
  }

  def parseInstructions(tokens: Seq[String]) : Seq[(Char, Int)] = {
    tokens.map(token => (token.charAt(0), token.substring(1).toInt))
  }

  def calculatePositions(directions: Seq[(Char, Int)]) : Seq[(Int, Int)] = {
    directions.foldLeft(Seq((0,0))) { case (positions, direction) =>
      val lastPosition = positions.head
      val newPositions = direction match {
        case ('D', distance) => (1 until distance).map(d => (lastPosition._1, lastPosition._2 - d))
        case ('U', distance) => (1 until distance).map(d => (lastPosition._1, lastPosition._2 + d))
        case ('R', distance) => (1 until distance).map(d => (lastPosition._1 + d, lastPosition._2))
        case ('L', distance) => (1 until distance).map(d => (lastPosition._1 - d, lastPosition._2))
        case _ => throw new Exception(s"Invalid direction $direction")
      }
      newPositions.reverse ++ positions
    }

  }
  def calculatManhattanDistance(position: (Int, Int)): Int = {
    Math.abs(position._1) + Math.abs(position._2)
  }

}
