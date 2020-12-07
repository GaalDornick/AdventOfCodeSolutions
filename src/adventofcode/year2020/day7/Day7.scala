package adventofcode.year2020.day7

import scala.io.Source

object Day7 {
  def main(args: Array[String]): Unit = {
    val rules = Source.fromFile("src/adventofcode/year2020/day7/input.txt").getLines().toSeq
    val parsedRules = rules.map(parse(_))

    val canGoIn = parsedRules.flatMap(rule => rule._2.map(t => (rule._1, t._1, t._2) ))
      .map(rule => rule._3 -> rule._1).toSeq
      .groupBy(_._1).mapValues(v => v.map(_._2))
    val result1 = findBagsForThisBag(canGoIn, "shiny gold").size
    println(result1)

    val result2 = numBagsContainedIn(parsedRules.toMap, "shiny gold")
    println(result2)

  }
  def findBagsForThisBag(canGoIn: Map[String, Seq[String]], bag: String) : Seq[String] = {
    val immediate = canGoIn.getOrElse(bag, Seq.empty[String])
    if(immediate.isEmpty) {
      immediate
    } else {
      (immediate ++ immediate.flatMap(findBagsForThisBag(canGoIn, _))).distinct
    }
  }
  def parse(str: String): (String, Seq[(Int, String)]) = {
    val format = """(.*) bags contain (.*).""".r
    str match {
      case format(container, contains) => {
        (container, contains.split(", ").toSeq.flatMap(parseContains(_)))

      }
      case _ => throw new Exception(s"$str couldn't parse")
    }
  }
  def parseContains(str: String): Seq[(Int, String)] = {
    val format = """(\d*) (.*) bags?""".r
    str match {
      case "no other bags" => Seq.empty
      case format(n, bag) => Seq((n.toInt, bag))
      case _ => throw new Exception(s"$str couldn't parse")
    }
  }

  def numBagsContainedIn(rules: Map[String, Seq[(Int, String)]], bag: String) : Long = {
    val childBags = rules.getOrElse(bag, throw new Exception(s"Couldn't find $bag"))
    childBags.map({ childBag =>
      val numChildBags = childBag._1
      val numChildOfChildBags = numChildBags * numBagsContainedIn(rules, childBag._2)
      numChildBags + numChildOfChildBags
    }).sum
  }
}
