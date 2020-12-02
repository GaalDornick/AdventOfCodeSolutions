package adventofcode.year2020.day2

import scala.io.Source
import scala.util.matching.Regex

object Day2 {
  def main(args: Array[String]): Unit = {
    val lines = Source.fromFile("src/adventofcode/year2020/day2/puzzleinput.txt").getLines()
    val parsed = lines.map(parse _).toSeq
    val validPasswords1 = parsed.map({ case(min, max, c, pass) =>
      (min, max, c, histogram(pass))
    }).map({ case(min, max, c, histogram) =>
      (min, max, c, histogram.getOrElse(c, 0))
    }).filter({ case(min, max, c, count) =>
      count >=min && count <=max
    })
    println(s"Number of valid passwords - first rule are ${validPasswords1.length}")
    val validPasswords2 = parsed.map({ case(one, two, c, pass) =>
      (c, if(one<=pass.length) pass.charAt(one-1) else Char.MinValue, if(two<=pass.length) pass.charAt(two-1) else Char.MinValue)
    }).filter({ case(c, first, second) =>
      (c==first && c!=second) || (c==second && c!=first)
    })
    println(s"Number of valid passwords - second rule are ${validPasswords2.length}")
  }
  def histogram(str: String): Map[Char, Int] = {
    val grouped = str.toCharArray.groupBy(c => c)
    grouped.mapValues(_.length)
  }
  def parse(s: String) : (Int, Int, Char, String) = {
    val lineFormat = """(\d*)-(\d*) (.): (.*)""".r
    s match {
      case lineFormat(min, max, c, pass) => (min.toInt, max.toInt, c.charAt(0), pass)
      case _ => throw new IllegalArgumentException(s"$s cannot be parsed")
    }
  }
}
