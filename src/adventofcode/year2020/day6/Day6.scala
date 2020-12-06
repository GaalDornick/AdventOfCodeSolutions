package adventofcode.year2020.day6

import scala.io.Source

object Day6 {
  def main(args: Array[String]): Unit = {
    val input = Source.fromFile("src/adventofcode/year2020/day6/input.txt").mkString
    val groups = input.split("\n\n")
    val result1 = groups.map(_.replace("\n",""))
      .map(_.toCharArray)
      .map(_.distinct)
      .map(_.size)
      .sum
    println(result1)
    println()
    val result2 = groups.map(_.split("\n"))
      .map(forms => forms.map(form => form.toCharArray))
      .map(forms => forms.reduceLeft({ (common, form) =>
        common.intersect(form)
      })).map(forms => forms.length).sum
    println(result2)
  }
}
