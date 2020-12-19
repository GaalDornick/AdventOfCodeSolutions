package adventofcode.year2020.day19

import scala.collection.mutable
import scala.io.Source
import scala.util.parsing.combinator.JavaTokenParsers

object Day19 {
  def main(args: Array[String]): Unit = {
    val lines = Source.fromFile("src/adventofcode/year2020/day19/input.txt").getLines()
    val rules = lines.takeWhile(_!="").toList
      .map(parseRule(_))
      .toMap

    val messages = lines.toList

    val match1 = messages.filter(m => doesMatch(m, rules(0), rules, 0).getOrElse(0)==m.size)
    println(match1)
    println(match1.size)
    //    val match2 = messages.filter(m => doesMatch(m, rules(0), rules.updated(8, "42 | 42 8").updated(11, "42 31 | 42 11 31"), 0).getOrElse(0)==m.size)
    //    println(match2)
    //    println(match2.size)
    val pattern42 = computeValidMessages("42", rules)
    val pattern31 = computeValidMessages("31", rules)
    println(s"42 => ${computeValidMessages("42", rules)}")
    println(s"31 => ${computeValidMessages("31", rules)}")
    val match2 = messages.filter(m => doesMatch2(m, pattern42, pattern31))
    println(match2)
    println(match2.size)



  }
  def doesMatch2(str: String, pattern42: Set[String], pattern31: Set[String]): Boolean = {
    val groups = str.grouped(pattern42.head.length).toSeq.reverse

    val lagging31s = groups.takeWhile(pattern31.contains(_))
    val remaining = groups.slice(lagging31s.size, groups.size)
    if(lagging31s.size ==0 || remaining.size<=lagging31s.size) false
    else {
      // all of them should be 42a
      remaining.map(pattern42.contains(_)).reduce(_ && _)
    }
  }
  val formatVal = """^"(.)"$""".r
  val formatOneRef = """^(\d*)$""".r
  val formatTwoRef = """^(\d*) (\d*)$""".r
  val formatThreeRef = """^(\d*) (\d*) (\d*)$""".r
  val formatChoice = """(.*) \| (.*)$""".r

  val memoizedResult =  mutable.HashMap.empty[(String, String), Option[Int]]
  def doesMatch(str: String, rule: String, allRules: Map[Int, String], level: Int) : Option[Int] = {
    val spaces = Seq.fill(level)("  ").mkString
    //println(s"$spaces$str -> $rule")
    val r = if(str.isEmpty) None else {
      memoizedResult.getOrElseUpdate((str, rule), {

        rule match {
          case formatVal(c) => if (str.startsWith(c)) Some(1) else None
          case formatOneRef(n) => doesMatch(str, allRules(n.toInt), allRules, level + 1)
          case formatTwoRef(n1, n2) => doesMatchMulti(str, Seq(n1, n2), allRules, level + 1)
          case formatThreeRef(n1, n2, n3) => doesMatchMulti(str, Seq(n1, n2, n3), allRules, level + 1)
          case formatChoice(n1, n2) => {
            val m1 = doesMatch(str, n1, allRules, level + 1)
            val m2 = doesMatch(str, n2, allRules, level + 1)
            (m1, m2) match {
              case (Some(r), None) => Some(r)
              case (None, Some(r)) => Some(r)
              case (Some(r1), Some(r2)) => Some(Math.max(r1, r2))
              case (None, None) => None
            }
          }
          case _ => throw new Exception(s"Invalid rule $rule")
        }

      })
    }
//    println(s"$spaces$str -> $rule => $r")
    r
  }

  def doesMatchMulti(str: String, rules: Seq[String], allRules: Map[Int, String], level:Int) : Option[Int] = {
    rules.tail.foldLeft(doesMatch(str, rules.head, allRules, level+1))((acc, n) => {
      acc.map(len => doesMatch(str.substring(len), n, allRules, level+1).map({
        len + _
      })).getOrElse(None)
    })
  }

  val memoizedValues = mutable.HashMap.empty[String, Set[String]]
  def computeValidMessages(rule: String, allRules: Map[Int, String]) : Set[String] = {
    memoizedValues.getOrElseUpdate(rule, {
      //println(s"===>$rule")

      rule match {
        case formatVal(c) => Set(c)
        case formatOneRef(n) => computeValidMessages(allRules(n.toInt), allRules)
        case formatTwoRef(n1, n2) => {
          val v1 = computeValidMessages(n1, allRules)
          val v2 = computeValidMessages(n2, allRules)
          cross(v1, v2)
        }
        case formatThreeRef(n1, n2, n3) => {
          val v1 = computeValidMessages(n1, allRules)
          val v2 = computeValidMessages(n2, allRules)
          val v3 = computeValidMessages(n3, allRules)
          cross(cross(v1, v2), v3)
        }

        case formatChoice(n1, n2) => {
          val v1 = computeValidMessages(n1, allRules)
          val v2 = computeValidMessages(n2, allRules)
          v1.union(v2)
        }
      }
    })
  }

  def parseRule(str: String): (Int ,  String) = {
    val format = """(\d*): (.*)""".r
    str match {
      case format(ruleNum, ruleExpr) => (ruleNum.toInt, ruleExpr)
    }
  }
  def cross(xs: Set[String], ys: Set[String]): Set[String] = {
    //println(s"${xs.size}x${ys.size}=${xs.size*ys.size}")
    for {x <- xs; y <- ys} yield s"$x$y"
  }

}




