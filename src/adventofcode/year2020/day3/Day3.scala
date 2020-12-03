package adventofcode.year2020.day3

import scala.io.Source

object Day3 {

  def main(args: Array[String]): Unit = {

    val steps = Seq((1,1),(3,1),(5,1),(7,1),(1,2))

    val bonks = steps.map({ step =>
      collisions(step._1, step._2)
    })
    println(bonks.mkString(","))
    val result = bonks.reduce(_*_)
    println(result)
  }

  private def collisions(rightStep: Int, downStep: Int) = {
    val trees = Source.fromFile("src/adventofcode/year2020/day3/input.txt").getLines().toSeq
    val height = trees.length
    val width = trees.head.length
    val tobPath = 0.until(height, downStep).map(n => (n -> n * rightStep / downStep)).toMap
    val result = tobPath.mapValues(_ % width)
      .map(normalizedPoint => normalizedPoint._1 -> trees(normalizedPoint._1).charAt(normalizedPoint._2))
      .filter(_._2 == '#')
      .size
    result
  }
}
