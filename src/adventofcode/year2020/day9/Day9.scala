package adventofcode.year2020.day9

import scala.io.Source

object Day9 {
  def main(args: Array[String]): Unit = {
    val numbers = Source.fromFile("src/adventofcode/year2020/day9/input.txt").getLines().toSeq
      .map(_.toLong)
    val invalidNumber = numbers.sliding(26) // put the list into groups of 6. The last elemeent should be sum of 2 elements out of the first 25
      .map(_.reverse) // reversed the groups so the sub it as head
      .map(group => (group.head, group.tail)) // seperated the head from the candidates
      .map(group => (group._1, group._2.combinations(2))) // created combinations of the 25 elemensts
      .map(group => (group._1, group._2.map(_.sum))) // addup every combination
      .filter(group => !group._2.contains(group._1)) // keep only the groups where the 6th element is not one of the combinations
      .map(_._1).toSeq // get only the number
      .head // take first one

    println(invalidNumber)

    // now find the numbers that add up to invalidnumber
    val weakeness = (2 to numbers.length).map { size =>
      numbers.sliding(size) // groups of 25 now
        .filter(_.sum == invalidNumber) // keep the group that adds up to invalid
        .map(group => group.min + group.max)
        .toSeq.headOption
    }.filter(_.isDefined)
        .map(_.get)
        .head
    println(weakeness)
  }
}
