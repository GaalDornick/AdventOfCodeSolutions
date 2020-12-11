package adventofcode.year2020.day11

import scala.io.Source



object Day11 {

  def main(args: Array[String]): Unit = {
    val seats = Source.fromFile("src/adventofcode/year2020/day11/input.txt").getLines()
      .map(_.toCharArray.toSeq).toSeq



    println(runSim(seats, false, 4))
    println(runSim(seats, true, 5))
  }
  def runSim(seats: Seq[Seq[Char]], lookFar: Boolean, occLimit: Int) : Int = {
    //var currentSeatValues = seats
    val height = seats.size
    val width = seats.head.size

    case class Point(x: Int, y: Int) {


    }
    val points = (0 until height).map { y =>
      (0 until width).map { x =>
        Point(x,y)
      }
    }
    val result = Stream.from(1).scanLeft(seats)({ (currentSeatValues, _) =>
      println("==>"+ currentSeatValues.map(_.filter(_=='#').size).sum)
      def tl(p: Point) = {
        if (!lookFar) {
          (if (p.x == 0 || p.y == 0) None else Some(Point(p.x - 1, p.y - 1))).map(seat(_))
        } else {
          (1 to Math.min(p.x, p.y)).map(n => Point(p.x - n, p.y - n)).map(seat(_)) // get all seats in this direction
            .find(_ != '.') // find the first seat that is not floor
        }
      }

      def t(p: Point) = {
        if (!lookFar) {
          (if (p.y == 0) None else Some(Point(p.x, p.y - 1))).map(seat(_))
        } else {
          (1 to p.y).map(n => Point(p.x, p.y - n)).map(seat(_)) // get all seats in this direction
            .find(_ != '.') // find the first seat that is not floor
        }
      }

      def tr(p: Point) = {
        if (!lookFar) {
          (if (p.x == width - 1 || p.y == 0) None else Some(Point(p.x + 1, p.y - 1))).map(seat(_))
        } else {
          (1 to Math.min(width - p.x - 1, p.y)).map(n => Point(p.x + n, p.y - n)).map(seat(_)) // get all seats in this direction
            .find(_ != '.') // find the first seat that is not floor
        }
      }

      def l(p: Point) = {
        if (!lookFar) {
          (if (p.x == 0) None else Some(Point(p.x - 1, p.y))).map(seat(_))
        } else {
          (1 to p.x).map(n => Point(p.x - n, p.y)).map(seat(_)) // get all seats in this direction
            .find(_ != '.') // find the first seat that is not floor
        }
      }

      def r(p: Point) = {
        if (!lookFar) {
          (if (p.x == width - 1) None else Some(Point(p.x + 1, p.y))).map(seat(_))
        } else {
          (1 to (width - p.x - 1)).map(n => Point(p.x + n, p.y)).map(seat(_)) // get all seats in this direction
            .find(_ != '.') // find the first seat that is not floor
        }
      }

      def bl(p: Point) = {
        if (!lookFar) {
          (if (p.x == 0 || p.y == height - 1) None else Some(Point(p.x - 1, p.y + 1))).map(seat(_))
        } else {
          (1 to Math.min(p.x, height - p.y - 1)).map(n => Point(p.x - n, p.y + n)).map(seat(_)) // get all seats in this direction
            .find(_ != '.') // find the first seat that is not floor
        }
      }

      def b(p: Point) = {
        if (!lookFar) {
          (if (p.y == height - 1) None else Some(Point(p.x, p.y + 1))).map(seat(_))
        } else {
          (1 to (height - p.y - 1)).map(n => Point(p.x, p.y + n)).map(seat(_)) // get all seats in this direction
            .find(_ != '.') // find the first seat that is not floor
        }
      }

      def br(p: Point) = {
        if (!lookFar) {
          (if (p.x == width - 1 || p.y == height - 1) None else Some(Point(p.x + 1, p.y + 1))).map(seat(_))
        } else {
          (1 to Math.min(width - p.x - 1, height - p.y - 1)).map(n => Point(p.x + n, p.y + n)).map(seat(_)) // get all seats in this direction
            .find(_ != '.') // find the first seat that is not floor
        }
      }

      def seat(p: Point) = currentSeatValues(p.y)(p.x)

      def adjSeats(p: Point) = {
        Seq(tl(p), t(p), tr(p), l(p), r(p), bl(p), b(p), br(p))
      }


      def mutate() = {
        points.map(row => {
          row.map { p =>
            (seat(p), adjSeats(p))
          }.map { case (seat, adjSeats) =>
            val countOccupied = adjSeats.filter(_ == Some('#')).size
            (seat, countOccupied)
          }.map { case (seat, count) =>
            seat match {
              case 'L' => if (count == 0) '#' else 'L'
              case '#' => if (count >= occLimit) 'L' else '#'
              case _ => seat
            }
          }.toSeq
        }).toSeq
      }

      mutate()
    }).sliding(2).map(pair => (pair.head, pair.tail.head))
      .takeWhile(p => !stringify(p._1).equals(stringify(p._2)))
      .map(_._2).toSeq.reverse.head
    result.map(_.filter(_=='#').size).sum
  }

  def stringify(seq: Seq[Seq[Char]]) = seq.map(_.mkString).mkString
}
