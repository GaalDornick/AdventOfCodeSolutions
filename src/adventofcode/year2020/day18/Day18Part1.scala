package adventofcode.year2020.day18

import scala.io.Source

object Day18 extends Arith1{

  def main(args: Array[String]): Unit = {
    val expressions = Source.fromFile("src/adventofcode/year2020/day18/input.txt").getLines().toSeq
    val parsed = expressions.map(parseAll(expr, _).get).sum

    println(parsed)
  }



}

import scala.util.parsing.combinator._
class Arith1 extends JavaTokenParsers
{
  def expr: Parser[Long] = term~rep("+"~term | "-"~term | "*"~term | "/"~term) ^^ {
    case number ~ list => list.foldLeft(number) {
      case(x, "+" ~ y ) => x + y
      case(x, "*" ~ y ) => x * y
    }
  }
  def term: Parser[Long] = num | "("~>expr<~")"
  def num : Parser[Long] = wholeNumber ^^ {
    case number => number.toLong
  }
  //def term: Parser[Any] = factor~rep("*"~factor | "/"~factor)
  //def factor: Parser[Any] = floatingPointNumber | "("~expr~")"
}
