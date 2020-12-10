package adventofcode.year2020.day10

import scala.io.Source

object Day10 {

  def main(args: Array[String]): Unit = {
    val joltages = Source.fromFile("src/adventofcode/year2020/day10/input.txt").getLines().toSeq
      .map(_.toInt)
    val deviceJoltage = (joltages.max + 3)
    val sortedJoltages = (0 +: joltages.sorted :+ deviceJoltage)
    println(sortedJoltages.mkString("\n"))

    val jumps = sortedJoltages.sliding(2)
      .map(j => j(1) - j(0)).toSeq
      .groupBy(diff => diff)
      .mapValues(_.size)
    println(jumps)
    println(jumps.getOrElse(1, 0) * jumps.getOrElse(3, 0))

    // group the joltages by contigious blocks
    val groupedContigious = sortedJoltages.tail.foldLeft(Seq(Seq(sortedJoltages.head)))({(acc, next) =>
      if(next - acc.reverse.head.reverse.head <3) {
        acc.slice(0, acc.size-1) :+(acc.reverse.head :+next)
      } else {
        acc :+ Seq(next)
      }
    }).map(group => (group, combinations(Seq(group.head), group.tail, group.reverse.head)))
        .map(_._2.toLong)
        .reduce(_*_)

    println(groupedContigious)


    val tail = sortedJoltages.tail
    //println(combinations(Seq(0), tail , deviceJoltage).size)
  }
  def combinations(connected: Seq[Int], available: Seq[Int], last: Int) : Int = {

    val lastConnectedJoltage = connected.reverse.head
    if (available.isEmpty) {
      if(lastConnectedJoltage==last) {
        println(s"${connected.mkString(",")}")
        1
      } else {
        0
      }
    } else {
      (1 to 3).map(lastConnectedJoltage + _)
        .filter(nextJoltage => available.contains(nextJoltage))
        .map({ nextJoltage =>
          val idxNextJoltage = available.indexOf(nextJoltage)
          combinations(connected :+ nextJoltage, available.slice(idxNextJoltage + 1, available.size), last)
        }).sum
    }
  }
}
