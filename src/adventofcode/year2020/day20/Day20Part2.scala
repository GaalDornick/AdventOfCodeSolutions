package adventofcode.year2020.day20

import scala.io.Source

object Day20Part2 {
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
    val grid = new Grid(tiles)
    while(!grid.isComplete) {
      println(grid.renderTileNum())
      grid.next()
      println
    }
    println(grid.renderTileNum())
  }
  def parseTile(lines: Seq[String]): Tile = {
    val hdrFormat = """Tile (\d*):""".r
    val tileNum = lines.head match {
      case hdrFormat(n) => n.toInt
      case _ => throw new Exception(s"Can't parse ${lines.head} as tile number")
    }
    Tile(tileNum, lines.tail)
  }
}

class Tile(val tileNum: Int, val lines: Seq[String]) {
  lazy val height = lines.size
  lazy val width = lines(0).size
  lazy val top = lines(0)
  lazy val bottom = lines.reverse.head
  lazy val left= lines.map(_(0)).mkString
  lazy val right = lines.map(_(width-1)).mkString
  lazy val topreverse = top.reverse
  lazy val bottomreverse = bottom.reverse
  lazy val leftreverse = left.reverse
  lazy val rightreverse = right.reverse
  //def edges = Seq(top, left, bottom, right)
  lazy val flipHoriz = Tile(tileNum, lines.reverse)
  lazy val flipVert = Tile(tileNum, lines.map(_.reverse))
  lazy val rotate90 = {
    Tile(tileNum, (0 until width).map({col =>
      (0 until height).map ({n =>
        val row = height-1-n
        lines(row)(col)
      }).mkString
    }))
  }
  lazy val rotate180 = rotate90.rotate90
  lazy val rotate270 = rotate180.rotate90


  def line(n: Int) = lines(n)

  def matchOther(other: Tile) : Option[(Tile, Int, Int)] = {
    (top, left, bottom, right) match {
      case (other.top, _, _, _) => Some((other.rotate180.flipVert, 0, -1))
      case (other.left, _, _, _) => Some((other.rotate270, 0, -1))
      case (other.bottom, _, _, _) => Some((other, 0, -1))
      case (other.right, _, _, _) => Some((other.rotate90, 0, -1))
      case (other.topreverse, _,_,_) => Some((other.rotate180, 0, -1))
      case (other.leftreverse, _,_,_) => Some((other.rotate270.flipVert, 0, -1))
      case (other.bottomreverse, _,_,_) => Some((other.flipVert, 0, -1))
      case (other.rightreverse, _,_,_) => Some((other.rotate90.flipVert, 0, -1))
      case (_, other.top, _, _) => Some((other.rotate90, -1, 0))
      case (_, other.left, _, _) => Some((other.flipVert, -1, 0))
      case (_, other.bottom, _, _) => Some((other.rotate90.flipVert, -1, 0))
      case (_, other.right, _, _) => Some((other, -1, 0))
      case (_, other.topreverse, _, _) => Some((other.rotate90.flipHoriz, -1, 0))
      case (_, other.leftreverse, _, _) => Some((other.flipVert.flipHoriz, -1, 0))
      case (_, other.bottomreverse, _, _) => Some((other.rotate90.flipVert.flipHoriz, -1, 0))
      case (_, other.rightreverse, _, _) => Some((other.flipHoriz, -1, 0))
      case (_, _, other.top, _) => Some((other, 0, 1))
      case (_, _, other.left, _) => Some((other.rotate90.flipVert, 0, 1))
      case (_, _, other.bottom, _) => Some((other.flipHoriz, 0, 1))
      case (_, _, other.right, _) => Some((other.rotate270, 0, 1))
      case (_, _, other.topreverse, _) => Some((other.flipVert, 0, 1))
      case (_, _, other.leftreverse, _) => Some((other.rotate90, 0, 1))
      case (_, _, other.bottomreverse, _) => Some((other.flipHoriz.flipVert, 0, 1))
      case (_, _, other.rightreverse, _) => Some((other.rotate270.flipVert, 0, 1))
      case (_, _, _, other.top) => Some((other.rotate90.flipVert, 1, 0))
      case (_, _, _, other.left) => Some((other, 1, 0))
      case (_, _, _, other.bottom) => Some((other.rotate90, 1, 0))
      case (_, _, _, other.right) => Some((other.rotate180.flipHoriz, 1, 0))
      case (_, _, _, other.topreverse) => Some((other.rotate90.flipVert.flipHoriz, 1, 0))
      case (_, _, _, other.leftreverse) => Some((other.flipHoriz, 1, 0))
      case (_, _, _, other.bottomreverse) => Some((other.rotate90.flipHoriz, 1, 0))
      case (_, _, _, other.rightreverse) => Some((other.rotate180, 1, 0))
      case _ => None
    }
  }
//  def mutations() : Set[Tile] = {
//    val rotations = (1 to 3).scanLeft(this)((a,_) => a.rotate90)
//    rotations.flatMap(t => {
//      Seq(t, t.flipHoriz, t.flipVert, t.flipVert.flipHoriz)
//    }).toSet
//  }
}


object Tile {
  def apply(tileNum: Int, lines: Seq[String]): Tile = new Tile(tileNum, lines)
}

class Grid(tiles:Seq[Tile]) {
  val cellSize = tiles.head.height
  val size = Math.sqrt(tiles.size).toInt
  var unarrangedTiles = tiles.tail.map(t => t.tileNum -> t).toMap
  var arrangedTiles = Map((0,0) -> tiles.head)
  var incompleteTiles = Seq((0,0, tiles.head))

  def nextIncomplete = incompleteTiles.head
  def isComplete = incompleteTiles.isEmpty


  def next() = {
    val (incompleteX, incompleteY, incompleteTile) = nextIncomplete
    val found = unarrangedTiles.values.map(incompleteTile.matchOther(_)).flatMap(_.map(Seq(_)).getOrElse(Seq.empty))
//    val allMutations = unarrangedTiles.values.flatMap({
//      _.mutations().map(m => m.tileNum -> m)
//    })
//    val offsets = Seq((0,-1), (-1,0), (0,1), (1,0))
//    val found = incompleteTile.edges.map(e => {
//      allMutations.filter(a=> a._2.edges.contains(e)).headOption
//    }).zip(offsets)

    //add all the found tiles to arranged tiles
    arrangedTiles = found.foldLeft(arrangedTiles)((acc, f) => {
      val(foundTile, xOffset, yOffset) = f
      val coords = (incompleteX+xOffset, incompleteY+yOffset)
      val present = acc.get(coords)
      if(present.isDefined) {

        throw new Exception(s"${coords} already contains ${present.get.tileNum}\n${incompleteTile.tileNum} trying to replace it with ${foundTile.tileNum}")
      }
      acc.updated(coords, foundTile)
    })
    // remove found tiles from unarranged tiles
    unarrangedTiles = found.foldLeft(unarrangedTiles)((acc, f) => {
      val(foundTile, _, _) = f
      acc.filter( _._1!=foundTile.tileNum)
    })
    //remove current tile from incomplete tiles
    incompleteTiles = incompleteTiles.filter(i => i._3.tileNum!=incompleteTile.tileNum)
    //add found tiles to incomplete tiles
    incompleteTiles = found.foldLeft(incompleteTiles)((acc, f) => {
      val(foundTile, xOffset, yOffset) = f
      acc :+ (incompleteX+xOffset, incompleteY+yOffset, foundTile)
    })
    println
  }

  def tile(x: Int, y: Int) = arrangedTiles.get(x, y)
  def tileNum(x:Int, y:Int) = arrangedTiles.get((x,y)).map(_.tileNum.toString).getOrElse("    ")
  def line(x: Int, y: Int, line: Int) = {
    tile(x, y).map(t => t.line(line)).getOrElse(Seq.fill(cellSize)(" ").mkString)
  }

  def renderTileNum() = {
    val minx = arrangedTiles.keys.map(_._1).min
    val maxx = arrangedTiles.keys.map(_._1).max
    val miny = arrangedTiles.keys.map(_._2).min
    val maxy = arrangedTiles.keys.map(_._2).max
    (miny to maxy).map(y => (minx to maxx).map(x => tileNum(x, y)).mkString("|")).mkString("\n")
  }
  def render() = {
    val minx = arrangedTiles.keys.map(_._1).min
    val maxx = arrangedTiles.keys.map(_._1).max
    val miny = arrangedTiles.keys.map(_._2).min
    val maxy = arrangedTiles.keys.map(_._2).max
    (for {y <- (miny to maxy); lineNum <- (0 until cellSize)} yield (y, lineNum))
      .map(yl => {
        val (y, lineNum) = yl
        (minx to maxx).map(x => line(x, y, lineNum)).reduce((acc, l) => acc.slice(0, acc.length-1)+l)
      }).mkString("\n")
  }


}

