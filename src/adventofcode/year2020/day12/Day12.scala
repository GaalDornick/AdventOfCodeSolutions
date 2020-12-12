package adventofcode.year2020.day12

import scala.io.Source

object Day12 {
  def main(args: Array[String]): Unit = {
    val directions = Source.fromFile("src/adventofcode/year2020/day12/input.txt").getLines()
      .map({d =>
        val format = """(.)(\d*)""".r
        d match {
          case format(direction, unit) => (direction.toCharArray.head, unit.toInt)
          case _ => throw new Exception(s"COuldn't parse $d")
        }
      }).toSeq
    val cardinals = Map('N' -> (0,1, 'E', 'W', 'S'),
      'S' -> (0,-1, 'W', 'E', 'N'),
      'E' -> (1,0, 'S', 'N', 'W'),
      'W' -> (-1,0, 'N', 'S', 'E'))
    val finalLoc1 = directions.foldLeft(('E', 0, 0))({(loc, direction) =>
      println(s"$loc -> $direction")
      direction match {
        case('N'|'S'|'E'|'W', n) =>(loc._1, loc._2+cardinals(direction._1)._1*n, loc._3+cardinals(direction._1)._2*n)
        case('F', n) =>(loc._1, loc._2+cardinals(loc._1)._1*n, loc._3+cardinals(loc._1)._2*n)
        case('L', 90) => (cardinals(loc._1)._4, loc._2, loc._3)
        case('R', 90) =>(cardinals(loc._1)._3, loc._2, loc._3)
        case('L', 270) => (cardinals(loc._1)._3, loc._2, loc._3)
        case('R', 270) =>(cardinals(loc._1)._4, loc._2, loc._3)
        case('L'|'R', 180) => (cardinals(loc._1)._5, loc._2, loc._3)
        case('L'|'R', 0) => (loc._1, loc._2, loc._3)
        case _ =>throw new Exception(s"Invalid direction $direction")
      }
    })
    println(finalLoc1)

    val finalLoc2 = directions.foldLeft(((0,0),(10,1))){case ((locShip, locWayPoint),direction) =>
      println(s"$locShip, $locWayPoint -> $direction")
      direction match {
        case('N'|'S'|'E'|'W', n) => (locShip, (locWayPoint._1+cardinals(direction._1)._1*n, locWayPoint._2+cardinals(direction._1)._2*n))
        case('F', n) => ((locShip._1+n*locWayPoint._1,locShip._2+n*locWayPoint._2),locWayPoint)
        case('L', 90) => (locShip, (-locWayPoint._2, locWayPoint._1))
        case('R', 270) =>(locShip, (-locWayPoint._2, locWayPoint._1))
        case('R', 90) => (locShip, (locWayPoint._2, -locWayPoint._1))
        case('L', 270) => (locShip, (locWayPoint._2, -locWayPoint._1))
        case('L'|'R', 180) => (locShip, (-locWayPoint._1, -locWayPoint._2))
        case('L'|'R', 0) => (locShip, locWayPoint)
        case _ =>throw new Exception(s"Invalid direction $direction")
      }
    }
    println(finalLoc2)
  }
}
