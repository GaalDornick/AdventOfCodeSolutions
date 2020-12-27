package adventofcode.year2020.day24

import java.awt.Polygon

import scala.io.Source

object Day24 {
  def main(args: Array[String]): Unit = {
    //run("src/adventofcode/year2020/day24/inputExample.txt")
    run("src/adventofcode/year2020/day24/input.txt")
  }

  val steps = Map("e" -> (0,1), "w"-> (0,-1), "ne"->(1,0), "nw"->(1,-1), "se" -> (-1, 1), "sw" -> (-1,0))

  def run(inputFile: String) = {
    val tileDirections = Source.fromFile(inputFile).getLines().toSeq

    val directionFormat = """[sn]?[ew]""".r
    val tileColors = tileDirections.map(tileDirection => {
      directionFormat.findAllIn(tileDirection)
        .map(steps(_))
        .foldLeft((0, 0))((current, step) => {
          (current._1 + step._1, current._2 + step._2)
        })
    }).groupBy(tile => tile)
      .mapValues(_.size%2)// if the tile is visited even times, it's white(0), otherwise it's black(1)
    val result1= tileColors.filter(_._2==1)// take only black tiles
      .size
    println(result1)
    renderAsImage(Map.empty[(Int, Int), Int], Map((0,0)->1, (0,1)->0, (-1,1)->1, (-1,0)->0, (0,-1)->1, (1,-1)->0, (1, 0) -> 1), "tileTest.png")
    renderAsImage(tileColors, tileColors, s"tiles0.png")
    val updatedGrid = (1 to 100).scanLeft(tileColors)((currentTiles, day) => {

      val mutatedTiles = mutateTiles( currentTiles)

      renderAsImage(currentTiles, mutatedTiles, s"tiles$day.png")
      mutatedTiles
    })
    val finalTiles = updatedGrid.reverse.head
    renderAsImage(finalTiles, finalTiles, s"tilesFinal.png")
    val dailyCount = updatedGrid.zipWithIndex.map(d => d._2 -> d._1.filter(_._2==1).size)

    println(dailyCount.map(d => s"Day${d._1}: ${d._2}").mkString("\n"))

  }

  def renderAsImage(prevTiles:Map[(Int, Int), Int],  currTiles: Map[(Int, Int), Int], file:String) = {
    val keys: Set[(Int, Int)] = (prevTiles.keys ++ currTiles.keys).toSet
    val tiles = keys.map(k => k -> {
        (prevTiles.getOrElse(k, 0), currTiles.getOrElse(k, 0))
      })
      .toMap


    import java.awt.image.BufferedImage
    import java.awt.Color

    val tileSize = 50
    val size = (5000, 5000)


    val canvas = new BufferedImage(size._1, size._2, BufferedImage.TYPE_INT_RGB)
    // get Graphics2D for the image
    val g = canvas.createGraphics()
    // clear background
    g.setColor(Color.GRAY)
    g.fillRect(0, 0, canvas.getWidth, canvas.getHeight)

    val R: Double = tileSize/2d
    val pointOffsets: Seq[(Double, Double)] = Seq((R, 0d),
      (R/2d, R*Math.sqrt(3d)/2d),
      (-R/2d, R*Math.sqrt(3d)/2d),
      (-R, 0d),
      (-R/2d, -R*Math.sqrt(3d)/2d),
      (R/2d, -R*Math.sqrt(3d)/2d))
    val tileWithDrawCoordinates = tiles.map(tile => {

      val r = R*Math.sqrt(3)/2f
      val xCenter = 2*tile._1._2*r + tile._1._1*r + size._1/2
      val yCenter = tile._1._1*Math.sqrt(3)*r + size._1/2
      (tile._1, tile._2, (yCenter, xCenter))
    }).toSeq
    val polygons = tileWithDrawCoordinates.map(tileWithCoords => {
      val h = new Polygon()
      val (_, _, coord) = tileWithCoords
      pointOffsets.foreach(po => {
        val(y, x) = add(coord, po)
        h.addPoint(x.toInt,y.toInt)
      })
      (h, tileWithCoords)
    })
    polygons.foreach(p => {
      val (polygon, ((y, x), c, (yDraw, xDraw))) = p
      g.setColor(Color.GREEN)
      g.drawPolygon(polygon)
      g.setColor(c match {
        case (0,0) => Color.WHITE
        case (1,1) => Color.BLACK
        case (0,1) => Color.GREEN
        case (1,0) => Color.RED
      })
      g.fillPolygon(polygon)
//      g.setColor(c match {
//        case (0,0) => Color.BLACK
//        case (1,1) => Color.WHITE
//        case (0,1) => Color.PINK
//        case (1,0) => Color.YELLOW
//      })
//
//      g.drawString(s"$y,$x", xDraw.toFloat-12f, (yDraw).toFloat)
//      g.setPaintMode()
    })


    javax.imageio.ImageIO.write(canvas, "png", new java.io.File(file))

  }

  private def mutateTiles(currentTiles: Map[(Int, Int), Int]): Map[(Int, Int), Int] = {
    val expandedTiles = currentTiles.keys.flatMap(getNeighbors(_)).toSet
    val expandedTilesWithColor = expandedTiles.map(t => (t, getTileColor(t, currentTiles)))
    // now mutate the tiles
    expandedTilesWithColor.map(tileColor => {
      val (tile, color) = tileColor
      val neighborWithColors = getNeighbors(tile).map(neighbor => (neighbor, getTileColor(neighbor, currentTiles)))
      val neighborColors = neighborWithColors.groupBy(_._2) // group neighbors by color
        .mapValues(_.size) // count how many colors
      val newColor = color match {
        case 1 => if (neighborColors.getOrElse(1,0) == 0 || neighborColors.getOrElse(1,0) > 2) 0 else 1
        case 0 => if (neighborColors.getOrElse(1,0) == 2) 1 else 0
      }
      (tile, newColor)
    }).filter(_._2==1)// keep only black because whites are not intersting
      .toMap
  }

  def getNeighbors(tile: (Int, Int)): Set[(Int, Int)] = {
    val r: Iterable[(Int, Int)] = steps.values.map(s => {
      val newCoord = (s._1 + tile._1, s._2 + tile._2)
      newCoord
    })
    r.toSet
  }
  def getTileColor(tile:(Int, Int), tileColors: Map[(Int, Int), Int]) = {
    tileColors.getOrElse(tile, 0)
  }

  def add(tuple1: (Double, Double), tuple2: (Double, Double)): (Double, Double) = {
    (tuple1._1+ tuple2._1, tuple1._2 + tuple2._2)
  }
}
