package adventofcode.year2020.day17

import scala.io.Source

object Day17PartN {


  def main(args: Array[String]): Unit = {
    val N = 6
    val grid = Source.fromFile("src/adventofcode/year2020/day17/input.txt").getLines().toSeq.zipWithIndex
      .flatMap(l => {
        val (line, y) = l
        line.zipWithIndex.map(c=> {
          val (cell, x) = c
          (Seq.fill(N-2)(0) ++ Seq(y,x)) -> (if( cell=='#') 1 else 0)
        })
      }).toMap
    val finalGrid = (1 to 6).foldLeft(grid)({ (acc, _) =>
      val expandedPoints = acc.keys.flatMap(neighbors(_)).toSet
      val r = expandedPoints.map(point => {
        val pointValue = valueForPoint(point, acc)
        val neighborForPoint = neighbors(point)
        val surroundingValues = neighborForPoint.map(valueForPoint(_, acc)).sum
        if(pointValue==1) {
          if(surroundingValues==4 || surroundingValues==3) {
            point -> 1
          } else {
            point -> 0
          }
        } else {
          point -> (if(surroundingValues==3) 1 else 0)
        }

      }).toMap
      println(s"===>${r.values.sum}")
      r
    })
    println(finalGrid.values.sum)
  }
  def valueForPoint(point: Seq[Int], grid: Map[Seq[Int], Int]) = {
    grid.getOrElse(point, 0)
  }
  def neighbors(point:Seq[Int]) : Seq[Seq[Int]] = {
    if(point.size==1) {
      (point.head - 1 to point.head+1).map(Seq(_))
    } else {
      (point.head - 1 to point.head+1).flatMap(h => neighbors(point.tail).map(h +: _))
    }
  }
}
