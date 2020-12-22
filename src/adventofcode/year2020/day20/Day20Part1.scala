package adventofcode.year2020.day20

import scala.io.Source

object Day20Part1 {

  def main(args: Array[String]): Unit = {
    val lines = Source.fromFile("src/adventofcode/year2020/day20/input.txt").getLines().toSeq

    val tiles= lines.foldLeft(Seq(Seq.empty[String]))({(acc, str) =>
      if(str.isEmpty) {
        acc :+ Seq.empty[String]
      } else {
        acc.updated(acc.size - 1, acc.reverse.head :+ str)
      }
    })
      .map(parseTile(_))
    val tileWithEdgePairs = tiles.map(extractEdges(_))
    val cornerTiles = tileWithEdgePairs.filter(tile => {

      val consTiles = tileWithEdgePairs.filter(_._1!=tile._1)
      val edgeMatches = consTiles.map(consTile => {
        adjoining(tile,consTile)

      }).reduce((t1, t2) => {
            (t1._1+t2._1, t1._2+t2._2, t1._3+t2._3, t1._4+t2._4)
          })

      edgeMatches match {
        case (0, 1, 1,0) => true
        case (0, 0, 1, 1) => true
        case (1, 0, 0, 1) => true
        case (1, 1, 0, 0) => true
        case _ => false
      }

    })
    println(cornerTiles.map(_._1.toLong).mkString(","))

    //part 2
    val gridSize = Math.sqrt(tiles.size)


  }

  def adjoining(tile1: (Int, Seq[String]), tile2: (Int, Seq[String])): (Int, Int, Int, Int) ={
    val m = tile1._2.map(e1=> {
      tile2._2.map(e2 => {
        if(e1 == e2 || e1 == e2.reverse) 1 else 0
      }).sum
    })
    (m(0), m(1), m(2), m(3))
  }

  def extractEdges(tile: (Int, Seq[((Int, Int), Char)])): (Int, Seq[String]) = {
    val height = tile._2.map(_._1._1).max
    val width = tile._2.map(_._1._2).max
    val top = tile._2.filter(_._1._1==0).sortBy(_._1._2).map(_._2).mkString
    val left = tile._2.filter(_._1._2==0).sortBy(_._1._1).map(_._2).mkString
    val bottom = tile._2.filter(_._1._1==height).sortBy(_._1._2).map(_._2).mkString
    val right = tile._2.filter(_._1._2==width).sortBy(_._1._1).map(_._2).mkString
    (tile._1, Seq(top, left, bottom, right))
//    // TODO maybe keepthis as seq of tuples instead of concat
//    (tile._1, Seq(left.reverse + top, bottom.reverse + left.reverse, right + bottom.reverse, top+right) )
  }
  private def parseTile(strTile: Seq[String]) : (Int, Seq[((Int, Int), Char)]) = {
    val hdrFormat = """Tile (\d*):""".r
    val tileNum = strTile.head match {
      case hdrFormat(n) => n.toInt
      case _ => throw new Exception(s"Can't parse ${strTile.head} as tile number")
    }
    (tileNum,
      strTile.tail.zipWithIndex
        .flatMap({ row =>
          row._1.zipWithIndex
            .map({ cell =>
             ((row._2, cell._2), cell._1)
            })
        })
    )
  }
}
