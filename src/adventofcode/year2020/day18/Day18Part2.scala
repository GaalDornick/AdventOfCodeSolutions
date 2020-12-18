package adventofcode.year2020.day18

import scala.io.Source

object Day18Part2 extends Arith2{

  def main(args: Array[String]): Unit = {
    println(parseAll(expr, "1 + 2 * 3").get)
    val expressions = Source.fromFile("src/adventofcode/year2020/day18/input.txt").getLines().toSeq
    val parsed = expressions.map(parseAll(expr, _).get).sum

    println(parsed)
  }



}

import scala.util.parsing.combinator._
class Arith2 extends JavaTokenParsers
{
  def expr: Parser[Long] = term~rep("*"~term ) ^^ {
    case number ~ list => list.foldLeft(number) {
      case(x, "*" ~ y ) => x * y
    }
  }
  def term: Parser[Long] = factor~rep("+"~factor) ^^ {
    case number ~ list => list.foldLeft(number) {
      case(x, "+" ~ y ) => x + y
    }
  }
  def factor: Parser[Long] = num | "("~>expr<~")"
  def num : Parser[Long] = wholeNumber ^^ {
    case number => number.toLong
  }
}
