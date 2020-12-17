package adventofcode.year2020.day17

import scala.io.Source


object Day17Part1 {

  def main(args: Array[String]): Unit = {
    val grid = Source.fromFile("src/adventofcode/year2020/day17/input.txt").getLines().toSeq.zipWithIndex
      .flatMap(l => {
        val (line, y) = l
        line.zipWithIndex.map(c=> {
          val (cell, x) = c
          (0, y, x) -> (if( cell=='#') 1 else 0)
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
  def valueForPoint(point: (Int, Int, Int), grid: Map[(Int, Int, Int), Int]) = {
    grid.getOrElse(point, 0)
  }
  def neighbors(point:(Int, Int, Int)) : Seq[(Int, Int, Int)] = {
    val (z, y, x) = point
    (z-1 to z+1).flatMap(nz =>{
      (y-1 to y+1).flatMap(ny => {
        (x-1 to x+1).map(nx =>{
          (nz, ny, nx)
        })
      })
    })
  }
}
